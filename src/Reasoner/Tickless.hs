{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reasoner.Tickless
    ( reasoner
    , WireAtom(..)
    , PhaseChange(..)
    , toWire
    , wire
    , unWire
    , parseWire
    ) where

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Logger.Timed
import Control.Monad.Logger.Prefix
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.State
import Data.EventTree (EventTree)
import Data.Bitraversable
import Data.Expr
import Data.Semigroup
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
import Reasoner.Tickless.Database
import Reasoner.Tickless.Translation
import Reasoner.Wire
import System.TimeIt
import System.ZMQ4 hiding (events)
import Text.Megaparsec (parse, errorBundlePretty)
import Numeric (showFFloat)
import Text.PrettyPrint.Leijen.Text
    ( displayTStrict
    , pretty
    , renderCompact
    , renderOneLine
    , sep
    , vcat
    )

import Clingo hiding (get)
import qualified Data.Bifunctor as BF
import qualified Language.LARS as LARS
import qualified Language.LARS.DependencyGraph as LARS
import qualified Language.ASP as ASP
import qualified Data.Set as S
import qualified Data.Text as T

-- | Parsing 'Text' in a 'MonadError' environment, throwing an error on parse
-- failure.
parse' :: MonadError Text m => LARS.Parser c -> Text -> m c
parse' p = liftEither . BF.first (pack . errorBundlePretty) . parse p "<memory>"

-- | C-esque interpretation of booleans for pure symbols
numBool :: PureSymbol -> Bool
numBool (PureNumber 0) = False
numBool _ = True

pureBasicAtom :: MonadError Text m => PureSymbol -> m LARS.BasicAtom
pureBasicAtom f@PureFunction {} =
    parse' LARS.atom . displayTStrict . renderOneLine . pretty $ f
pureBasicAtom x =
    throwError $ "Error while parsing basic atom from " <> pack (show x)

type Timer = MVar (Maybe (Min UTCTime))

noteTimer :: MonadIO m => Timer -> Maybe UTCTime -> m ()
noteTimer timer t = liftIO $ modifyMVar_ timer (pure . (<> (Min <$> t)))

-- | Reset a timer. This is NOT atomic!
resetTimer :: MonadIO m => Timer -> m ()
resetTimer timer = liftIO $ tryTakeMVar timer *> putMVar timer Nothing

-- | The injector function determines how external functions are to be handled
-- during grounding time. It is defined over all available window atoms to
-- reduce them into ASP expressions.
injector ::
       (MonadError Text m, MonadReasonerDB m, MonadIO m)
    => Timer
    -> UTCTime
    -> Location
    -> Text
    -> [PureSymbol]
    -> m [PureSymbol]
injector timer now _ "atW" [a@PureFunction {}, PureNumber {}, PureString w] = do
    a' <- LARS.ground <$> pureBasicAtom a
    w' <- parse' LARS.window w
    times <- atW now a' w'
    when (not . null $ times) $
        latest a' >>= \case
            Just (TimeInterval _ (Simple ee)) ->
                let next = fmap (`addUTCTime` ee) (LARS.windowSize w')
                 in noteTimer timer next
            _ -> pure ()
    pure $ map (PureString . pack . show) times
injector timer now _ "always" [a@PureFunction {}, t@PureNumber {}, PureString w] = do
    a' <- LARS.ground <$> pureBasicAtom a
    w' <- parse' LARS.window w
    r <- always now a' w'
    when (not r) $
        latest a' >>= \case
            Just (TimeInterval es Infinite) ->
                let next = fmap (`addUTCTime` es) (LARS.windowSize w')
                 in noteTimer timer next
            _ -> pure ()
    pure $
        if r && numBool t
            then [PureNumber 1]
            else [PureNumber 0]
injector timer now _ "happened" [a@PureFunction {}, t@PureNumber {}, PureString w] = do
    a' <- LARS.ground <$> pureBasicAtom a
    w' <- parse' LARS.window w
    r <- happened now a' w'
    when r $
        latest a' >>= \case
            Just (TimeInterval _ (Simple ee)) ->
                let next = fmap (`addUTCTime` ee) (LARS.windowSize w')
                 in noteTimer timer next
            _ -> pure ()
    pure $
        if r && numBool t
            then [PureNumber 1]
            else [PureNumber 0]
injector _ _ _ _ _ = throwError "Unknown function"

reason ::
       ( MonadLogger m
       , MonadMask m
       , MonadReasonerDB m
       , Connection m ~ EventTree Text
       , MonadIO m
       )
    => Timer
    -> Text -- ^ Rendered Program
    -> m [PureSymbol]
reason timer p = do
    resetTimer timer
    tree <- snapshot
    ms <-
        withDefaultClingo $ do
            now <- liftIO getCurrentTime
            addProgram "base" ([] :: [a]) p
            ground
                [Part "base" []]
                (Just $ \l t xs ->
                     runEventTreeT' tree (runExceptT $ injector timer now l t xs))
            withSolver [] (allModels >=> traverse pureModel)
    case ms of
        [] -> do
            logWarnN "Empty model!"
            pure []
        m:_ -> pure m

pureModel :: (Monad (m s), MonadModel m) => Model s -> m s [PureSymbol]
pureModel m = map toPureSymbol <$> modelSymbols m selectAll

reasoner' ::
       ( MonadError (ReasonerError TranslationError) m
       , MonadLogger m
       , MonadIO m
       , MonadMask m
       , MonadReasonerDB m
       , Connection m ~ EventTree Text
       , MonadReader Sockets m
       , MonadState (S.Set LARS.GroundAtom) m
       )
    => LARS.Program
    -> [String]
    -> String
    -> S.Set LARS.BasicAtom
    -> S.Set LARS.BasicAtom
    -> m ()
reasoner' p inputs output listening publishing = do
    logDebugN $
        "Using program\n" <>
        displayTStrict (renderCompact . vcat . map pretty $ p)
    logInfoN "Translating program"
    p' <- liftEither . BF.first TranslationError . translate $ p
    -- set up ZMQ sockets
    setupSockets inputs output
    let filts = listening
        wants = LARS.want p
    logDebugN $
        "Reasoner listening to " <>
        pack (show . map filterString . S.toList $ listening)
    logDebugN $
        "Reasoner publishing " <>
        pack (show . map filterString . S.toList $ publishing)
    -- timer MVar
    timer <- liftIO $ newMVar Nothing
    -- main loop
    logInfoN "Tickless Reasoner ready"
    forever $ do
        trigger <- triggerTimer timer filts
        (t, ()) <- timeItT $ do
            passthrough <-
                case trigger of
                    Left evs -> do
                        logDebugN $
                            "Received from network: " <>
                            T.unwords (map (decodeUtf8 . toWire) evs)
                        now <- liftIO getCurrentTime
                        logDebugN $
                            "Processing " <> pack (show . length $ evs) <>
                            " new events"
                    -- register events
                        events
                            now
                            (map unWire .
                             filter (\(WireAtom _ a) -> LARS.unground a `S.member` wants) $
                             evs)
                        logDebugN $ "Events registered."
                    -- passthrough events
                        pure $ filter (wireMatch publishing) evs
                    Right () -> logDebugN "Timer expired, solving..." *> pure mempty
            -- compute answer set
            answer <- getAnswerSet timer p'
            logDebugN $
                "Answer set found: " <>
                (displayTStrict . renderOneLine . sep . map pretty . S.toList $
                 answer)
            changeset <- calculateChangeset answer
            publishChanges (changeset <> passthrough)
        logDebugN $ "Reasoner CPU Time: " <> pack (showFFloat (Just 6) t "s")

triggerTimer ::
       ( MonadReader Sockets m
       , MonadLogger m
       , MonadError (ReasonerError t) m
       , MonadIO m
       )
    => Timer
    -> S.Set LARS.BasicAtom
    -> m (Either [WireAtom] ())
triggerTimer next filts = do
    next' <- liftIO $ readMVar next
    case next' of
        Nothing -> do
            evs <-
                asks subscriber >>= \sub -> do
                    logDebugN "Waiting for messages..."
                    msgs <- liftIO $ receiveMulti sub
                    filter (wireMatch filts) <$>
                        traverse parseWire msgs
            case evs of
                [] -> do
                    logDebugN "No relevant message received in last batch."
                    triggerTimer next filts
                _ -> pure $ Left evs
        Just next'' -> do
            sub <- asks subscriber
            logDebugN $
                "Waiting for messages or timer interrupt (" <>
                pack (show . getMin $ next'') <>
                ")..."
            r <-
                liftIO $ do
                    now <- getCurrentTime
                    let delta =
                            truncate . (* (1.0e6 :: Double)) . realToFrac $
                            diffUTCTime (getMin next'') now
                    timer <- async (threadDelay delta)
                    network <- async $ receiveMulti sub
                    waitEither network timer
            r' <-
                bitraverse
                    (fmap (filter (wireMatch filts)) .
                     traverse parseWire)
                    pure
                    r
            case r' of
                Left [] -> do
                    logDebugN "No relevant message received in last batch."
                    triggerTimer next filts
                _ -> pure $ r'

-- | Compute an answer set given the current solver state and the translated
-- program.
getAnswerSet ::
       ( MonadError (ReasonerError t) m
       , MonadLogger m
       , MonadMask m
       , MonadReasonerDB m
       , MonadIO m
       , Connection m ~ EventTree Text
       )
    => Timer
    -> ASP.Program
    -> m (S.Set LARS.GroundAtom)
getAnswerSet timer p =
    fmap S.fromList . traverse symbolToAtom =<<
    reason timer (displayTStrict . renderOneLine . mconcat . map pretty $ p)

symbolToAtom :: MonadError (ReasonerError t) m => PureSymbol -> m LARS.GroundAtom
symbolToAtom (PureFunction t xs s) =
    case (t, xs) of
        ("at", [PureString x, PureString n]) ->
            LARS.AtAtom <$> strAtom x <*> pure (String n)
        _ ->
            LARS.Atom . LARS.GroundBasicAtom . LARS.BasicAtom s t <$>
            traverse arg xs
  where
    arg (PureNumber n) = pure . Constant . fromIntegral $ n
    arg (PureString x) = pure . String $ x
    arg _ = throwError $ AnswerSetError "Unsupported argument"
    strAtom x =
        case parse LARS.atom "<memory>" x of
            Left _ -> throwError $ AnswerSetError "Invalid contained atom"
            Right a -> pure (LARS.GroundBasicAtom a)
symbolToAtom _ = throwError $ AnswerSetError "Non-function symbol encountered"

reasoner ::
       Text
    -> LARS.Program
    -> [String]
    -> String
    -> S.Set LARS.BasicAtom
    -> S.Set LARS.BasicAtom
    -> IO ()
reasoner pre p i o listening publishing =
    runReasoner pre (reasoner' p i o listening publishing)

type ReasonerM
     = ExceptT (ReasonerError TranslationError) (EventTreeT (LogPrefixT 
        (LoggingT (StateT (S.Set LARS.GroundAtom) (ReaderT Sockets IO)))))

runReasoner :: Text -> ReasonerM () -> IO ()
runReasoner pre k =
    sockets $ \pub sub ->
        let logged =
                runExceptT k >>= \case
                    Left e -> logErrorR e *> pure ()
                    Right _ -> pure ()
         in runReaderT
                (evalStateT
                     (runTimedStderrLoggingT
                          (prefixLogs pre (runEventTreeT logged)))
                     mempty)
                (Sockets pub sub)
