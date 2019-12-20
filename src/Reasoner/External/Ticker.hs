{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Reasoner.External.Ticker
    ( Ticker
    , TickerReasoner(..)
    , withTicker
    , tick
    , getAnswer
    ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Time (DiffTime)
import Data.Void
import System.IO (Handle, hSetBuffering, BufferMode(..), hFlush)
import System.Process
import System.Posix.Signals

import qualified Data.Text.IO as TIO
import qualified Language.LARS as LARS
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

data TickerReasoner = TickerJTMS | TickerClingo
    deriving (Eq, Show, Read, Ord)

tickerReasoner :: TickerReasoner -> String
tickerReasoner TickerJTMS = "incremental"
tickerReasoner TickerClingo = "clingo"

data Ticker = Ticker
    { tickerInput :: Handle
    , tickerOutput :: Handle
    , tickerHandle :: ProcessHandle
    }

-- | Obtain a temporary file, via @mktemp@.
mktemp :: MonadIO m => m FilePath
mktemp = liftIO $ head . lines <$> readProcess "mktemp" [] ""

-- | Spawn a 'Ticker'. Assumes java to be installed on the system, and will
-- /not/ perform checks for this!
spawnTicker ::
       MonadIO m
    => FilePath -- ^ Path to Ticker .jar
    -> Text -- ^ Printed LARS program
    -> DiffTime
    -> TickerReasoner
    -> Bool
    -> m Ticker
spawnTicker tickerPath program clock reasoner debugMode =
    liftIO $
     do
        -- Write program to temporary file
        progFile <- mktemp
        TIO.writeFile progFile program
        -- Spawn ticker
        (Just tickerStdin, Just tickerStdout, _, hdl) <-
            createProcess $
            (proc
                 "java"
                 ([ "-jar"
                  , tickerPath
                  , "-r"
                  , tickerReasoner reasoner
                  , "-p"
                  , progFile
                  , "-c"
                  , show clock
                  ] <>
                  if debugMode
                      then ["-l", "debug"]
                      else []))
                {std_in = CreatePipe, std_out = CreatePipe}
        hSetBuffering tickerStdin NoBuffering
        pure $ Ticker tickerStdin tickerStdout hdl

endTicker :: MonadIO m => Ticker -> m ()
endTicker (Ticker _ _ hdl) = liftIO $ terminateProcess hdl

withTicker ::
       (MonadMask m, MonadIO m)
    => FilePath
    -> Text
    -> DiffTime
    -> TickerReasoner
    -> Bool
    -> (Ticker -> m b)
    -> m b
withTicker tickerPath program clock reasoner debugMode k =
    spawnTicker tickerPath program clock reasoner debugMode >>= \t -> do
        _ <- liftIO . async $ waitForProcess (tickerHandle t)
        _ <-
            liftIO $
            installHandler
                userDefinedSignal1
                (Catch (endTicker t))
                Nothing
        x <- finally (k t) (endTicker t)
        pure x

tick :: MonadIO m => Ticker -> NonEmpty LARS.GroundBasicAtom -> m ()
tick Ticker {..} evs =
    let evs' =
            PP.displayTStrict .
            PP.renderOneLine .
            PP.hcat . PP.punctuate PP.semi . toList . fmap PP.pretty $
            evs
     in liftIO $ do
            TIO.hPutStrLn tickerInput evs'
            hFlush tickerInput

type Parser = P.Parsec Void Text

data TickerLine =
    TickerLine !DiffTime
               [LARS.GroundBasicAtom]

tickerLine :: Parser TickerLine
tickerLine =
    TickerLine <$> (time <* P.skipSomeTill P.anySingle (P.char ':') <* P.space1) <*>
    answerset
  where
    time = fromInteger <$> L.decimal
    answerset :: Parser [LARS.GroundBasicAtom]
    answerset =
        P.string "Set" *>
        P.between
            (P.char '(')
            (P.char ')')
            ((LARS.ground <$> LARS.atom) `P.sepBy` (P.char ',' *> P.space))

getAnswer :: MonadIO m => Ticker -> m [LARS.GroundBasicAtom]
getAnswer ticker@(Ticker {..}) = do
    line <- liftIO $ TIO.hGetLine tickerOutput
    case P.parse tickerLine "<ticker>" line of
        Left _ -> do
            liftIO . TIO.putStrLn $ "Unparsed Ticker Output: " <> line
            getAnswer ticker
        Right (TickerLine _ xs) -> pure xs
