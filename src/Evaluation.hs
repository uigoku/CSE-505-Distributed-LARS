{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Evaluation
    ( evaluate
    , limit
    , Evaluation
    , mmedia
    , contentRetrieval
    , EvaluateOpts
    , evaluateOpts
    ) where

import Control.Lens hiding (argument)
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.Time
import GHC.Generics
import Options.Applicative
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Process.Stat

import Evaluation.ContentRetrieval
import Evaluation.MMedia
import Evaluation.Types
import Evaluation.NQueens
import Simulation

import qualified Text.PrettyPrint.Leijen.Text as PP

data TestCase
    = MMedia Bool
    | ContentRetrieval
    | NQueens Int
    deriving (Show, Generic)

data EvaluateOpts
    = TotalTimeTest TotalTimeTestOpts
    | LoadTest LoadTestOpts
    | DumpCase TestCase
               Int
               Bool
    deriving (Show, Generic)

data LoadTestOpts = LoadTestOpts
    { ltTestCase :: TestCase
    , ltWindowSize :: Int
    , msgIntervalUpper :: Double
    , msgIntervalLower :: Double
    , msgIntervalStep :: Double
    , ltSimOpts :: SimulateOpts
    } deriving (Show, Generic)

data TotalTimeTestOpts = TotalTimeTestOpts
    { ttTestCase :: TestCase
    , ttSimOpts :: SimulateOpts
    , ttWindowSize :: Int
    , ttEvents :: Maybe Int
    } deriving (Show, Generic)

testCaseR :: ReadM TestCase
testCaseR =
    maybeReader $ \case
        "mmedia" -> Just $ MMedia False
        "mmedia-at" -> Just $ MMedia True
        "content-retrieval" -> Just ContentRetrieval
        "nqueens-2" -> Just $ NQueens 2
        "nqueens-4" -> Just $ NQueens 4
        "nqueens-6" -> Just $ NQueens 6
        _ -> Nothing

loadTestOpts :: Parser LoadTestOpts
loadTestOpts =
    LoadTestOpts <$> argument testCaseR (metavar "TESTCASE") <*>
    option
        auto
        (short 'w' <> long "window" <> help "Window size to use in test case" <>
         value 10) <*>
    option
        auto
        (short 'u' <> help "Upper limit of message interval, default = 1.0" <>
         value 1.0) <*>
    option
        auto
        (short 'l' <> help "Lower limit of message interval, default = 1.0-e9" <>
         value 1.0e-9) <*>
    option
        auto
        (long "step" <> help "Interval step size, default = 0.1" <> value 0.1) <*>
    simulateOpts

totalTimeTestOpts :: Parser TotalTimeTestOpts
totalTimeTestOpts =
    TotalTimeTestOpts <$> argument testCaseR (metavar "TESTCASE") <*>
    simulateOpts <*>
    option
        auto
        (short 'w' <> help "Window size for test (default = 1)" <> value 1) <*>
    optional
        (option auto (short 'e' <> help "Number of events to test"))

evaluateOpts :: Parser EvaluateOpts
evaluateOpts =
    subparser
        (command
             "load-test"
             (info
                  (LoadTest <$> loadTestOpts <**> helper)
                  (progDesc "Run load test")) <>
         command
             "total-time"
             (info
                  (TotalTimeTest <$> totalTimeTestOpts <**> helper)
                  (progDesc "Run total time test")) <>
         command
             "dump-case"
             (info
                  (DumpCase <$> argument testCaseR (metavar "TESTCASE") <*>
                   option auto (short 'l' <> help "Test log length" <> value 50) 
                   <*> switch (short 'e' <> help "Dump encoding instead") <**>
                   helper)
                  (progDesc "Dump a test case log")))

evaluate :: MonadIO m => EvaluateOpts -> m ()
evaluate (DumpCase testCase n e) = do
    let Evaluation p es _ _ _ =
            limit n $
            case testCase of
                MMedia False -> mmedia 1 Nothing
                MMedia True -> mmediaTicker 1 Nothing
                ContentRetrieval -> contentRetrieval 1
                NQueens stages -> nqueens 18 stages
    if e
        then liftIO . PP.putDoc . PP.vcat . map PP.pretty $ p
        else liftIO . PP.putDoc . PP.vcat . map (PP.hcat . map PP.pretty) $ scanLog es
evaluate (TotalTimeTest opts) = do
    let e =
            case ttTestCase opts of
                MMedia False -> mmedia (ttWindowSize opts) Nothing
                MMedia True -> mmediaTicker (ttWindowSize opts) Nothing
                ContentRetrieval -> contentRetrieval (ttWindowSize opts)
                NQueens stages -> nqueens 18 stages
    liftIO $ hPutStrLn stderr "Running total time test..."
    tt <- runExceptT $ totalTimeTest e (ttSimOpts opts) (ttEvents opts)
    liftIO $
        case tt of
            Left err -> hPutStrLn stderr err *> exitFailure
            Right xs -> do
                hPutStrLn stderr "Total time test successful!"
                hPutStrLn stderr "Reasoner times:"
                traverse_ (hPutStrLn stderr . prettyProcTimes) xs
    liftIO $ exitSuccess
evaluate (LoadTest opts) = do
    let e =
            case ltTestCase opts of
                MMedia False -> mmedia (ltWindowSize opts) Nothing
                MMedia True -> mmediaTicker (ltWindowSize opts) Nothing
                ContentRetrieval -> contentRetrieval (ltWindowSize opts)
                NQueens stages -> nqueens 18 stages
    liftIO $ hPutStrLn stderr "Running load test..."
    lt <-
        runExceptT $
        loadTest
            (limit 100 e)
            (ltSimOpts opts)
            (msgIntervalUpper opts)
            (msgIntervalLower opts)
            (msgIntervalStep opts)
    liftIO $
        case lt of
            Left err -> hPutStrLn stderr err *> exitFailure
            Right xs ->
                hPutStrLn stderr "Load test successful!" *>
                hPutStrLn
                    stderr
                    (unlines $
                     map
                         (\(tick, lat) ->
                              "At tick interval " <> show tick <>
                              " latency was " <>
                              show lat)
                         xs)
    liftIO $ exitSuccess

loadTest ::
       (MonadError String m, MonadIO m)
    => Evaluation
    -> SimulateOpts
    -> Double
    -> Double
    -> Double
    -> m [(DiffTime, DiffTime)]
loadTest (Evaluation p i _ f extDt) so upper lower step =
    let runs = map (toDT . max lower) [upper,upper - step .. lower]
     in do liftIO . hPutStrLn stderr $
               "Performing " <> show (length runs) <> " runs"
           traverse (\dt -> (dt, ) <$> go dt) runs
  where
    toDT :: Double -> DiffTime
    toDT = picosecondsToDiffTime . truncate . (* 1.0e12)
    go :: (MonadError String m, MonadIO m) => DiffTime -> m DiffTime
    go tickLength = do
        let stream = zip (repeat tickLength) i
        stats <-
            simulate
                (setExternalClock (fromMaybe tickLength extDt) so)
                (ListenerTerminated f)
                p
                stream
        let ni = sumOf (nIn . _head . sampleTime) stats
            no = sumOf (nOut . _head . sampleTime) stats
        pure $ no - ni

totalTimeTest ::
       (MonadError String m, MonadIO m)
    => Evaluation
    -> SimulateOpts
    -> Maybe Int
    -> m [ProcTimes]
totalTimeTest e so evs =
    let es = map (`limit` e) (maybe [100,200 .. 1000] pure evs)
     in map (fold . view reasonerTimes) <$> traverse go es
  where
    go :: (MonadError String m, MonadIO m) => Evaluation -> m SimStatistics
    go (Evaluation p s _ _ _) = do
        liftIO $ do
            hPutStrLn stderr "Evaluating program"
            PP.hPutDoc stderr . PP.vcat . map PP.pretty $ p
            hPutStrLn stderr $ "\nStream Length: " <> show (length s)
        simulate so (MasterTerminated 0) p (zip (repeat 0.1) s)
