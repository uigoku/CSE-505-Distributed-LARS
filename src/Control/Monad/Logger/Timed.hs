{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Logger.Timed
    ( runTimedStderrLoggingT
    ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Time
import System.IO (Handle, stderr)
import System.Log.FastLogger (fromLogStr)

import qualified Data.ByteString.Char8 as S8

defaultTimedOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultTimedOutput h loc src lvl str = do
    now <- getCurrentTime
    let str' = defaultLogStr loc src lvl str
        timed =
            formatTime
                defaultTimeLocale
                (iso8601DateFormat (Just "%T.%q"))
                now
    S8.hPutStr h . fromLogStr $ "[" <> toLogStr timed <> "] " <> str'

runTimedStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runTimedStderrLoggingT = (`runLoggingT` defaultTimedOutput stderr)
