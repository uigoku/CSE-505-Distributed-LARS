{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Graph.Inductive
import GHC.Generics
import Options.Applicative
import System.Process (callProcess)
import Text.PrettyPrint.Leijen.Text (displayTStrict, pretty, renderPretty, vcat)

import Evaluation
import Reasoner.Tickless.Translation
import Simulation

import Language.LARS hiding (Parser)
import Language.LARS.DependencyGraph

import qualified Data.Text.IO as T

data GraphType
    = DependencyGraph
    | ComponentGraph
    deriving (Show)

data ShowGraphOpts = ShowGraphOpts
    { graphType :: GraphType
    , produceSingleNodeGraph :: Bool
    } deriving (Show, Generic)

showGraphOpts :: Parser ShowGraphOpts
showGraphOpts =
    ShowGraphOpts <$>
    argument
        (maybeReader $ \case
             "dep" -> Just DependencyGraph
             "comp" -> Just ComponentGraph
             _ -> Nothing)
        (metavar "GRAPH") <*>
    switch (short 's' <> help "Produce single node graph")

data Command
    = ShowGraph ShowGraphOpts FilePath
    | Linearize FilePath
    | TranspileOnly FilePath
    | Simulate SimulateOpts FilePath
    | Evaluate EvaluateOpts
    deriving (Show, Generic)

data Options = Options
    { optCommand :: Command
    } deriving (Show, Generic)

options :: Parser Options
options =
    Options <$>
    subparser
        (command
             "show-graph"
             (info
                  (ShowGraph <$> showGraphOpts <*> file <**> helper)
                  (progDesc "Show graph")) <>
         command
             "linearize"
             (info
                  (Linearize <$> file <**> helper)
                  (progDesc "Linearize a LARS program")) <>
         command
             "transpile"
             (info
                  (TranspileOnly <$> file <**> helper)
                  (progDesc "Transpile to ASP and dump program")) <>
         command
             "simulate"
             (info
                  (Simulate <$> simulateOpts <*> file <**> helper)
                  (progDesc "Run simulation")) <>
         command
             "evaluate"
             (info
                  (Evaluate <$> evaluateOpts <**> helper)
                  (progDesc "Run evaluation")))
  where
    file = argument str (metavar "FILE")

exceptMaybe :: MonadError e m => e -> Maybe a -> m a
exceptMaybe e Nothing = throwError e
exceptMaybe _ (Just a) = pure a

-- | Runner for the 'ShowGraph' command. Depending on options it will either
-- show the dependency graph or the component graph. It assumes @xdot@ to be
-- installed!
showGraph ::
       (MonadError String m, MonadIO m) => Program -> ShowGraphOpts -> m ()
showGraph p (ShowGraphOpts DependencyGraph _) = do
    let path = "/tmp/depgraph.dot"
        g = fst $ dGraph p
    liftIO $ do
        writeFile path (dotGraph g)
        callProcess "xdot" [path]
showGraph p (ShowGraphOpts ComponentGraph single) = do
    let path = "/tmp/compgraph.dot"
        dg = fst $ dGraph p
    g <- if single
            then pure (singleNodeGraph p)
            else exceptMaybe "Cycles detected in component graph" $ componentGraph p dg
    liftIO $ do
        writeFile path (dotGraph . nmap pretty $ g)
        callProcess "xdot" [path]

-- | Runner for the linearization command. Linearizes the program, printing out
-- its transpiled stages.
linearize' :: (MonadError String m, MonadIO m) => Program -> m ()
linearize' p = do
    s <- exceptMaybe "Linearization failed" $ linearize p
    liftIO .
        traverse_ (\(n :: Int, x) -> print n *> T.putStrLn (prettyProgram x)) .
        zip [1 ..] $
        s

-- | Runner for the 'TranspileOnly' command. It transpiles the program in full
-- and prints it to stdout.
transpileOnly :: (MonadError String m, MonadIO m) => Program -> m ()
transpileOnly p = do
    p' <- liftEither . first show . translate $ p
    liftIO .
        T.putStrLn . displayTStrict . renderPretty 0.9 120 . vcat . map pretty $
        p'

main :: IO ()
main = do
    Options {..} <-
        execParser $
        info (options <**> helper) (fullDesc <> progDesc "dynacon-prototype")
    r <-
        runExceptT $ do
            case optCommand of
                Simulate opts prog -> do
                    p <- readProgram prog
                    l <- readLog
                    void $ simulate opts (MasterTerminated 3.0) p l
                ShowGraph opts prog -> do
                    p <- readProgram prog
                    showGraph p opts
                Linearize prog -> do
                    p <- readProgram prog
                    linearize' p
                TranspileOnly prog -> do
                    p <- readProgram prog
                    transpileOnly p
                Evaluate opts -> evaluate opts
    case r of
        Left e -> putStrLn $ "Error: " <> e
        _ -> pure ()
