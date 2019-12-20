{-# LANGUAGE FlexibleContexts #-}
module Language.LARS
    ( module Language.LARS.AST
    , module Language.LARS.Parser
    , readProgram
    , maxTime
    ) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Lens
import Control.Lens.Extras
import Data.Bifunctor
import Language.LARS.AST
import Language.LARS.Parser

import qualified Data.Text.IO as T
import qualified Text.Megaparsec as Megaparsec

maxTime :: [Window] -> Maybe Window
maxTime = maximumOf traverse . filter (is _SlidingTimeWindow)

readProgram :: (MonadError String m, MonadIO m) => FilePath -> m Program
readProgram fp = do
    f <- liftIO $ T.readFile fp
    case first Megaparsec.errorBundlePretty . Megaparsec.parse program fp $ f of
        Left e -> throwError e
        Right x -> pure x
