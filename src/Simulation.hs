{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Simulation
    ( SimulateOpts(..)
    , simulateOpts
    , setExternalClock
    , TerminationMode(..)
    , simulate
    , readLog
    -- * Components
    , spawn
    , spawnGraph
    , master
    , terminateAll
    -- * Statistics
    , SimStatistics(..)
    , nIn
    , nOut
    , reasonerTimes
    , Sample(..)
    , sampleTime
    , sample
    ) where

import Control.Lens (makeLenses, over, (<|), Lens', set)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Logger.Prefix
import Control.Monad.Logger.Timed
import Data.Bifunctor
import Data.Foldable
import Data.Graph.Inductive
import Data.Functor.Identity
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Time
import Data.Void
import GHC.Generics
import Language.LARS hiding (Parser)
import Language.LARS.DependencyGraph
import Options.Applicative hiding (empty)
import Reasoner.Wire (WireAtom, toWire, unWire, parseWire, wire)
import Reasoner
import System.Posix.Process (forkProcess)
import System.Posix.Signals (signalProcess, softwareTermination, userDefinedSignal1)
import System.Posix.Types (ProcessID)
import System.Process.Stat
import System.ZMQ4.Monadic hiding (async, events)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Set as S

data Sample a = Sample 
    { _sampleTime :: !DiffTime
    , _sample :: !a }
    deriving (Eq, Show, Ord)

makeLenses ''Sample

data SimStatistics = SimStatistics
    { _nIn :: [Sample Int]
    , _nOut :: [Sample Int]
    , _reasonerTimes :: [ProcTimes]
    } deriving (Eq, Show, Ord)

makeLenses ''SimStatistics

data SimulateOpts = SimulateOpts
    { singleNode :: Bool
    , reasonerOptions :: ReasonerOpts
    } deriving (Show, Generic)

simulateOpts :: Parser SimulateOpts
simulateOpts =
    SimulateOpts <$>
    switch
        (short 's' <> long "single" <> help "Run simulation in single node mode")
        <*>
        reasonerOpts

data ReasonerOpts
    = TicklessOpts
    | TickerOpts { tickerPath :: FilePath
                 , tickTime :: DiffTime
                 , tickerDebug :: Bool
                 , tickerReasoner :: TickerReasoner }
    deriving (Show, Generic)

setExternalClock :: DiffTime -> SimulateOpts -> SimulateOpts
setExternalClock clock so = so {reasonerOptions = go (reasonerOptions so)}
  where
    go TicklessOpts = TicklessOpts
    go o@TickerOpts {} = o {tickTime = clock}

tickerReasonerR :: ReadM TickerReasoner
tickerReasonerR = maybeReader $ \case
    "incremental" -> Just TickerJTMS
    "clingo" -> Just TickerClingo
    _ -> Nothing

reasonerOpts :: Parser ReasonerOpts
reasonerOpts = tickerOpts <|> pure TicklessOpts
  where
    tickerOpts =
        TickerOpts <$>
        strOption
            (long "ticker" <> metavar "JAR" <>
             help "If using Ticker, path to Ticker JAR file") <*>
        option
            (picosecondsToDiffTime . truncate . (* 1.0e12) <$> auto @Double)
            (long "tick-interval" <> metavar "SEC" <>
             help "If using Ticker, tick interval in seconds") <*>
        switch
            (long "ticker-debug" <>
             help "If using Ticker, run Ticker with Debug Output") <*>
        option
            tickerReasonerR
            (long "ticker-reasoner" <> help "If using Ticker, reasoner to use" <>
             value TickerJTMS)

-- | Determine port string from a node number
address :: Bool -> Node -> String
address listening n =
    (if listening
         then "tcp://*:"
         else "tcp://localhost:") <>
    (show $ n + 5500)

masterAddress :: Bool -> String
masterAddress listening = address listening 0

-- | Spawn a reasoner process given a list of initial nodes (to check whether
-- this node is initial) and the context of the node in the graph.
spawn ::
       (Foldable t, MonadIO m)
    => Reasoner
    -> t Node
    -> Context (NetworkNode Identity) [BasicAtom]
    -> m (Maybe ProcessID)
spawn _ _ (_, _, MasterNode _, _) = pure Nothing
spawn r inits (inputs, n, Reasoner prog pub, _) =
    let pres = map (\(_, x) -> address False x) inputs
        mlink = [masterAddress False | n `elem` inits]
        output = address True n
        listening = foldMap (S.fromList . fst) inputs
        publishing = runIdentity pub
     in Just <$> liftIO
            (forkProcess $
             reasoner
                r
                 (T.pack . address False $ n)
                 prog
                 (pres <> mlink)
                 output
                 listening
                 publishing)

-- | Spawns a network from the component graph
spawnGraph :: MonadIO f => ComponentGraph -> Reasoner -> f [ProcessID]
spawnGraph g r =
    let inits = sourceNodes g
     in fmap catMaybes . traverse (spawn r inits) . map (context g) . nodes $ g

-- | Runs a master node
master ::
       MonadIO m
    => UTCTime
    -> Maybe DiffTime
    -> [(DiffTime, NonEmpty WireAtom)]
    -> MVar SimStatistics
    -> m ()
master startTime term inp stats = do
    runZMQ $ do
        publisher <- socket Pub
        bind publisher (masterAddress True)
        runTimedStderrLoggingT . prefixLogs "Master" $ do
            logInfoN "Waiting 3s for startup"
            liftIO $ threadDelay 3.0e6
            runQueue (map (second (sendInp publisher)) inp)
            case term of
                Just timeout -> do
                    logInfoN "Waiting for shutdown"
                    liftIO $ threadDelay (diffTimeToMicro timeout)
                Nothing -> pure ()
  where
    sendInp publisher xs = do
        let wired = toWire <$> xs
        logInfoN $ "Sending " <> T.pack (show (toList wired))
        lift . lift $ sendMulti publisher wired
        writeStat stats startTime nIn 1

emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics [] [] []

writeStat :: (MonadIO m) => MVar s -> UTCTime -> Lens' s [Sample b] -> b -> m ()
writeStat stats startTime l k =
    liftIO $ do
        now <- getCurrentTime
        let t = ignoreNominal $ diffUTCTime now startTime
            smp = Sample t k
        modifyMVar_ stats (pure . over l (smp <|))

listener ::
       MonadIO m
    => Maybe GroundBasicAtom
    -> UTCTime
    -> [String]
    -> MVar SimStatistics
    -> m ()
listener terminator startTime sinks stats =
    runZMQ $ do
        runTimedStderrLoggingT . prefixLogs "Listener" $ do
            subscriber <-
                lift . lift $ do
                    s <- socket Sub
                    traverse_ (connect s) sinks
                    subscribe s ""
                    pure s
            logInfoN "Listener started"
            logDebugN $ "Subscribed to " <> T.pack (show sinks)
            case terminator of
                Nothing -> forever (go subscriber)
                Just x ->
                    let loop = do
                            xs <- go subscriber
                            case traverse (fmap (fst . unWire) . parseWire) xs of
                                Left _ ->
                                    logErrorN "Error while parsing wire atoms" *>
                                    loop
                                Right xs'
                                    | x `elem` xs' -> pure ()
                                    | otherwise -> loop
                     in loop
  where
    go subscriber = do
        xs <- lift . lift $ receiveMulti subscriber
        logInfoN $ "Received " <> T.pack (show xs)
        writeStat stats startTime nOut (length xs)
        pure xs

terminateAll :: (MonadIO m, Traversable f) => f ProcessID -> m (f ProcTimes)
terminateAll pids =
    liftIO $
    traverse
        (\pid -> do
             times <- getProcTimes pid
             signalProcess userDefinedSignal1 pid
             -- give process time to process signal
             threadDelay 5000000
             signalProcess softwareTermination pid
             pure times)
        pids

data TerminationMode
    = MasterTerminated DiffTime
    | ListenerTerminated GroundBasicAtom
    deriving (Eq, Show, Ord)

masterTimeOut :: TerminationMode -> Maybe DiffTime
masterTimeOut (MasterTerminated t) = Just t
masterTimeOut _ = Nothing

listenerTerminator :: TerminationMode -> Maybe GroundBasicAtom
listenerTerminator (ListenerTerminated a) = Just a
listenerTerminator _ = Nothing

simulate ::
       (MonadError String m, MonadIO m)
    => SimulateOpts
    -> TerminationMode
    -> Program
    -> [(DiffTime, NonEmpty WireAtom)]
    -> m SimStatistics
simulate opts term p stream = do
    g <-
        if singleNode opts
            then pure $ singleNodeGraph p
            else maybe (throwError "Cycles detected in component graph") pure $
                 componentGraph p (fst $ dGraph p)
    -- fork off child processes, collecting PIDs
    let r = case reasonerOptions opts of
                TicklessOpts -> Tickless
                TickerOpts path dt dbg tr -> External (Ticker path dt tr dbg)
    pids <- spawnGraph g r
    -- start listener in thread, main thread continues to master
    stats <- liftIO $ newMVar emptyStatistics
    startTime <- liftIO $ getCurrentTime
    listener' <-
        liftIO . async $
        listener
            (listenerTerminator term)
            startTime
            (map (address False) (sinkNodes g))
            stats
    -- master stays alive to process log file and send events
    master startTime (masterTimeOut term) stream stats
    -- after termination, send SIGTERM to all child processes
    liftIO $
        case term of
            ListenerTerminated _ -> do
                timeout <- async (threadDelay 120.0e6)
                void $ waitEither timeout listener'
            _ -> pure ()
    times <- terminateAll pids
    -- terminate listener and obtain statistics for return
    stats' <- liftIO $ cancel listener' *> readMVar stats
    pure (set reasonerTimes times stats')

-- | Reads a stream log from stdin
readLog :: MonadIO m => m [(DiffTime, NonEmpty WireAtom)]
readLog =
    deltas . sortOn fst . mapMaybe (P.parseMaybe logEntry) . T.lines <$>
    liftIO T.getContents
  where
    logEntry = (,) <$> (utcTime <* sc) <*> (fromList <$> P.sepBy1 wire P.space)

ignoreNominal :: NominalDiffTime -> DiffTime
ignoreNominal = picosecondsToDiffTime . truncate @Double . (* 1.0e12) . realToFrac

deltas :: [(UTCTime, a)] -> [(DiffTime, a)]
deltas xs =
    let ts = map fst xs
        ps = (0 :) . map ignoreNominal . zipWith diffUTCTime (tail ts) $ ts
     in zip ps (map snd xs)

diffTimeToMicro :: DiffTime -> Int
diffTimeToMicro = fromIntegral . (`div` 1.0e6) . diffTimeToPicoseconds

runQueue :: (MonadLogger m, MonadIO m) => [(DiffTime, m ())] -> m ()
runQueue =
    traverse_ $ \(t, k) -> do
        let t' = diffTimeToMicro t
        logInfoN $ "Waiting " <> T.pack (show t) <> "."
        liftIO (threadDelay t')
        logInfoN "Executing"
        k

utcTime :: P.Parsec Void T.Text UTCTime
utcTime = do
    tks <- T.unpack <$> P.takeP Nothing 19
    parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) tks
