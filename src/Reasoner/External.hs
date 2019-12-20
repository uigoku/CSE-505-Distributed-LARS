{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Reasoner.External
    ( ExternalReasoner(..)
    , reasoner
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Logger.Prefix
import Control.Monad.Logger.Timed
import Control.Monad.Reader
import Control.Monad.State
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Text (Text, pack)
import Data.Time (DiffTime, diffTimeToPicoseconds)
import Data.Void
import Reasoner.External.Ticker
import Reasoner.Tickless.Database (PhaseChange(..))
import Reasoner.Wire
import System.ZMQ4 (Pub, Socket, Sub, receiveMulti)
import Text.PrettyPrint.Leijen.Text
    ( displayTStrict
    , pretty
    , renderCompact
    , renderOneLine
    , vcat
    )

import qualified Data.Set as S
import qualified Language.LARS as LARS
import qualified Language.LARS.DependencyGraph as LARS

data ExternalReasoner =
    Ticker FilePath
           DiffTime
           TickerReasoner
           Bool

reasoner ::
       ExternalReasoner
    -> Text
    -> LARS.Program
    -> [String]
    -> String
    -> S.Set LARS.BasicAtom
    -> S.Set LARS.BasicAtom
    -> IO ()
reasoner r prefix program inputs outputs listening publishing =
    sockets $ \pub sub ->
        runReaderT
            (runTimedStderrLoggingT
                 (prefixLogs
                      prefix
                      (reasoner'
                           r
                           prefix
                           program
                           inputs
                           outputs
                           listening
                           publishing)))
            (Sockets pub sub)

reasoner' ::
       ( MonadMask m
       , MonadLogger m
       , MonadReader Sockets m
       , Foldable t
       , MonadIO m
       )
    => ExternalReasoner
    -> Text
    -> LARS.Program
    -> t String
    -> String
    -> S.Set LARS.BasicAtom
    -> S.Set LARS.BasicAtom
    -> m ()
reasoner' r pre prog is os listening publishing = do
    let tickerProg
                -- Ticker's parser cannot handle -a, so we rename it.
         =
            displayTStrict
                (renderCompact . vcat . map pretty . LARS.renameNegation $ prog)
    logDebugN $ "Using program\n" <> tickerProg
    -- set up ZMQ sockets
    setupSockets is os
    let filts = LARS.want prog
    logDebugN $
        "Reasoner listening to " <>
        pack (show . map filterString . S.toList $ listening)
    logDebugN $
        "Reasoner publishing " <>
        pack (show . map filterString . S.toList $ publishing)
    -- set up passthrough channel
    passthrough <- liftIO newChan
    case r of
        Ticker path dt tickerR debugMode ->
            withTicker path tickerProg dt tickerR debugMode $ \ticker -> do
                logInfoN "External Reasoner Ticker ready"
                Sockets pub sub <- ask
                liftIO $ do
                    l <-
                        async $
                        externalListener
                            (tick ticker)
                            (pre <> " Listener")
                            dt
                            filts
                            publishing
                            passthrough
                            sub
                    p <-
                        async $
                        externalPublisher
                            (getAnswer ticker)
                            (pre <> " Publisher")
                            passthrough
                            pub
                    void $ waitEither l p

wireSet :: Ord a => [(a, PhaseChange)] -> S.Set a -> S.Set a
wireSet xs z =
    let (pos, neg) = partition ((== Positive) . snd) xs
     in z `S.union` S.fromList (map fst pos) `S.difference`
        S.fromList (map fst neg)

externalListener ::
       (NonEmpty LARS.GroundBasicAtom -> IO ())
    -> Text -- ^ Logging Prefix
    -> DiffTime -- ^ Tick Time
    -> S.Set LARS.BasicAtom -- ^ Wants set
    -> S.Set LARS.BasicAtom -- ^ Publishing Set
    -> Chan [WireAtom] -- ^ Passthrough channel to publisher
    -> Socket Sub -- ^ Subscriber socket
    -> IO a
externalListener push pre res wants publishing passthrough sub = do
    current <- newMVar mempty
    _ <- forkIO $ runTimedStderrLoggingT (prefixLogs pre (receiver current))
    runTimedStderrLoggingT . prefixLogs pre . forever $ do
        liftIO $
            threadDelay
                (fromIntegral . (`div` 1.0e6) . diffTimeToPicoseconds $ res)
        as <- liftIO $ readMVar current
        case nonEmpty . S.toList $ as of
            Nothing -> logDebugN "Empty or busy input stream"
            Just xs -> do
                logDebugN $
                    "Pushing " <>
                    (displayTStrict . renderOneLine . pretty . toList $ xs) <>
                    " to external reasoner"
                liftIO $ push xs
  where
    receiver current =
        forever $ do
            raw <- liftIO $ receiveMulti sub
            evs <-
                runExceptT @(ReasonerError Void) $ do
                    parsed <- traverse parseWire $ raw
                    let here = map unWire . filter (wireMatch wants) $ parsed
                        there = filter (wireMatch publishing) parsed
                    pure (here, there)
            case evs of
                Left e -> logErrorR e
                Right (xs, pt) -> liftIO $ do
                    takeMVar current >>= putMVar current . wireSet xs
                    writeChan passthrough pt

externalPublisher ::
       (IO [LARS.GroundBasicAtom])
    -> Text
    -> Chan [WireAtom]
    -> Socket Pub
    -> IO a
externalPublisher fetch pre passthrough pub = do
    _ <- forkIO $ runTimedStderrLoggingT (prefixLogs pre $ pt)
    runTimedStderrLoggingT (prefixLogs pre $ evalStateT go S.empty)
  where
    go =
        forever $ do
            xs <- liftIO fetch
            logDebugN $
                "Received " <> (displayTStrict . renderOneLine . pretty $ xs) <>
                " from external reasoner"
            changes <-
                runExceptT @(ReasonerError Void) $
                calculateChangeset (S.fromList $ map LARS.Atom xs)
            case changes of
                Left e -> logErrorR e
                Right cs -> publishChanges' cs pub
    pt =
        forever $ do
            xs <- liftIO $ readChan passthrough
            logDebugN $
                "Passing through " <>
                (displayTStrict . renderOneLine . pretty . map (fst . unWire) $
                 xs)
            publishChanges' xs pub
