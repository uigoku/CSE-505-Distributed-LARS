{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LARS.TH
    ( lars
    , atom
    , gatom
    , rule
    ) where

import Data.Data
import Data.Expr
import Data.Generics
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Quote
import Text.Megaparsec (errorBundlePretty, parse)

import qualified Data.Text as T
import qualified Language.LARS.Parser as LARS
import qualified Language.LARS.AST as LARS

liftText :: T.Text -> Maybe (Q Exp)
liftText txt = Just (AppE (VarE 'T.pack) <$> lift (T.unpack txt))

larsP :: (Data a, Lift a) => LARS.Parser a -> String -> Q Exp
larsP p str = do
    filename <- loc_filename <$> location
    case parse p filename (pack str) of
        Left err -> error (errorBundlePretty err)
        Right x -> dataToExpQ (const Nothing `extQ` antiValue `extQ` liftText) x

antiValue :: Value -> Maybe (Q Exp)
antiValue (Antiquoted v) = Just embed
  where
    embed = varE (mkName v)
antiValue _ = Nothing

lars :: QuasiQuoter
lars = QuasiQuoter (larsP LARS.program) err err err
  where
    err = error "Not defined"

atom :: QuasiQuoter
atom = QuasiQuoter (larsP LARS.atom) err err err
  where
    err = error "Not defined"

gatom :: QuasiQuoter
gatom = QuasiQuoter (larsP $ LARS.ground <$> LARS.atom) err err err
  where
    err = error "Not defined"

rule :: QuasiQuoter
rule = QuasiQuoter (larsP LARS.rule) err err err
  where
    err = error "Not defined"
