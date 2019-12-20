{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Reasoner.Tickless.Translation
    ( translate
    , TranslationError(..)
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Expr
import Data.Text (Text)

import Text.PrettyPrint.Leijen.Text (Pretty(..), displayTStrict, renderOneLine)

import qualified Language.ASP as ASP
import qualified Language.LARS as LARS
import Prelude hiding (head)

data TranslationError =
    MalformedHead LARS.Head
    deriving (Show)

show' :: Pretty a => a -> Text
show' = displayTStrict . renderOneLine . pretty

-- | Find a free signature for @at@ atoms.
findAt :: ASP.Signature
findAt = ASP.Signature "at" 2

-- | Associate a basic atom.
basic :: LARS.BasicAtom -> ASP.Atom
basic (LARS.BasicAtom True n vs) = ASP.Atom n vs
basic (LARS.BasicAtom False n vs) =
    ASP.Negated . basic $ LARS.BasicAtom True n vs

atom :: LARS.Atom -> ASP.BodyElement
atom (LARS.Atom b) = ASP.BodyAtom $ basic b
atom (LARS.AtAtom b t) =
    ASP.BodyRelation $ ArithValue t `RelEq` ArithFunc "at" [String $ show' b]
atom (LARS.AtWAtom b t i w) =
    ASP.BodyRelation $
    ArithValue t `RelEq`
    ArithFunc
        "atW"
        [ UnsafeUnquoted $ show' b
        , Constant $
          if i
              then 1
              else 0
        , String $ show' w
        ]
atom (LARS.AlwaysAtom b i w) =
    ASP.BodyRelation $
    1 `RelEq`
    ArithFunc
        "always"
        [ UnsafeUnquoted $ show' b
        , Constant $
          if i
              then 1
              else 0
        , String $ show' w
        ]
atom (LARS.HappenedAtom b i w) =
    ASP.BodyRelation $
    1 `RelEq`
    ArithFunc
        "happened"
        [ UnsafeUnquoted $ show' b
        , Constant $
          if i
              then 1
              else 0
        , String $ show' w
        ]

body :: LARS.BodyElement -> ASP.BodyElement
body (LARS.BodyAtom a) = atom a
body (LARS.BodyRelation r) = ASP.BodyRelation r

head ::
       (MonadReader ASP.Signature m, MonadError TranslationError m)
    => LARS.Head
    -> m ASP.Atom
head (LARS.Head (LARS.AtAtom b t)) = do
    ASP.Signature at _ <- ask
    pure $ ASP.Atom at [String . show' $ b, t]
head (LARS.Head (LARS.Atom b)) = pure $ basic b
head h = throwError $ MalformedHead h

larsToAspRules ::
       (MonadReader ASP.Signature m, MonadError TranslationError m)
    => LARS.Rule
    -> m ASP.Rule
larsToAspRules (LARS.Rule h bs) = ASP.Rule <$> head h <*> pure (map body bs)

translate :: MonadError TranslationError m => LARS.Program -> m ASP.Program
translate p = runReaderT (traverse larsToAspRules . LARS.allRules $ p) findAt
