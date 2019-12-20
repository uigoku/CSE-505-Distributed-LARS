{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Expr
    ( Expr(..)
    , Value(..)
    , Arith(..)
    , Relation(..)
    ) where

import Data.Set (Set)
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Language.Haskell.TH.Lift

import Instances.TH.Lift ()

import qualified Data.Set as S

class Expr a where
    variableFree :: a -> Bool
    variables :: a -> Set Text
    substitute :: Text -> Double -> a -> a
    reduce :: a -> a

data Arith
    = Add Arith
          Arith
    | Subtract Arith
               Arith
    | Divide Arith
             Arith
    | Multiply Arith
               Arith
    | Modulo Arith
             Arith
    | Exp Arith
          Arith
    | ArithValue Value
    | ArithFunc Text [Value]
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Num Arith where
    (+) = Add
    (-) = Subtract
    (*) = Multiply
    abs = id
    signum = id
    fromInteger = ArithValue . Constant . fromInteger

instance Fractional Arith where
    (/) = Divide
    fromRational = ArithValue . Constant . fromRational

instance Expr Arith where
    variableFree (Add a b) = variableFree a && variableFree b
    variableFree (Subtract a b) = variableFree a && variableFree b
    variableFree (Divide a b) = variableFree a && variableFree b
    variableFree (Multiply a b) = variableFree a && variableFree b
    variableFree (Modulo a b) = variableFree a && variableFree b
    variableFree (Exp a b) = variableFree a && variableFree b
    variableFree (ArithValue v) = variableFree v
    variableFree (ArithFunc _ _) = True
    variables (Add a b) = variables a <> variables b
    variables (Subtract a b) = variables a <> variables b
    variables (Divide a b) = variables a <> variables b
    variables (Multiply a b) = variables a <> variables b
    variables (Modulo a b) = variables a <> variables b
    variables (Exp a b) = variables a <> variables b
    variables (ArithValue v) = variables v
    variables (ArithFunc _ _) = mempty
    substitute v d (Add a b) = Add (substitute v d a) (substitute v d b)
    substitute v d (Subtract a b) =
        Subtract (substitute v d a) (substitute v d b)
    substitute v d (Divide a b) = Divide (substitute v d a) (substitute v d b)
    substitute v d (Multiply a b) =
        Multiply (substitute v d a) (substitute v d b)
    substitute v d (Modulo a b) = Modulo (substitute v d a) (substitute v d b)
    substitute v d (Exp a b) = Exp (substitute v d a) (substitute v d b)
    substitute v d (ArithValue x) =
        case x of
            Variable v'
                | v' == v -> ArithValue (Constant d)
                | otherwise -> ArithValue (Variable v')
            Constant c -> ArithValue (Constant c)
            String c -> ArithValue (String c)
            UnsafeUnquoted c -> ArithValue (UnsafeUnquoted c)
            Antiquoted c -> ArithValue (Antiquoted c)
    substitute _ _ x@ArithFunc{} = x
    reduce (Add (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant $ a + b)
    reduce (Add a b) = reduce $ Add (reduce a) (reduce b)
    reduce (Subtract (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant $ a - b)
    reduce (Subtract a b) = reduce $ Subtract (reduce a) (reduce b)
    reduce (Divide (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant $ a / b)
    reduce (Divide a b) = reduce $ Divide (reduce a) (reduce b)
    reduce (Multiply (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant $ a * b)
    reduce (Multiply a b) = reduce $ Multiply (reduce a) (reduce b)
    reduce (Modulo (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant . fromIntegral @Int $ truncate a `mod` truncate b)
    reduce (Modulo a b) = reduce $ Modulo (reduce a) (reduce b)
    reduce (Exp (ArithValue (Constant a)) (ArithValue (Constant b))) =
        ArithValue (Constant $ a ** b)
    reduce (Exp a b) = reduce $ Exp (reduce a) (reduce b)
    reduce a = a

arithPrec :: Arith -> Int
arithPrec ArithValue{} = 4
arithPrec ArithFunc{} = 4
arithPrec Exp{} = 3
arithPrec Modulo{} = 2
arithPrec Divide{} = 2
arithPrec Multiply{} = 2
arithPrec Add{} = 1
arithPrec Subtract{} = 1

parensIfPrec :: Int -> Arith -> Doc -> Doc
parensIfPrec k x d
    | arithPrec x < k = parens d
    | otherwise = d

instance Pretty Arith where
    pretty (Add a b) =
        parensIfPrec 1 a (pretty a) <+> char '+' <+> parensIfPrec 1 b (pretty b)
    pretty (Subtract a b) =
        parensIfPrec 1 a (pretty a) <+> char '-' <+> parensIfPrec 1 b (pretty b)
    pretty (Divide a b) =
        parensIfPrec 2 a (pretty a) <+> char '/' <+> parensIfPrec 2 b (pretty b)
    pretty (Multiply a b) =
        parensIfPrec 2 a (pretty a) <+> char '*' <+> parensIfPrec 2 b (pretty b)
    pretty (Modulo a b) =
        parensIfPrec 2 a (pretty a) <+> char '%' <+> parensIfPrec 2 b (pretty b)
    pretty (Exp a b) =
        parensIfPrec 3 a (pretty a) <+> char '^' <+> parensIfPrec 3 b (pretty b)
    pretty (ArithValue v) = pretty v
    pretty (ArithFunc f xs) =
        char '@' <> pretty f <>
        (enclose lparen rparen . hcat . punctuate (char ',') . map pretty $ xs)

data Relation
    = RelEq Arith
            Arith
    | RelNeq Arith
             Arith
    | RelGEq Arith
             Arith
    | RelLEq Arith
             Arith
    | RelGt Arith
            Arith
    | RelLt Arith
            Arith
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Expr Relation where
    variableFree (RelEq l r) = variableFree l && variableFree r
    variableFree (RelNeq l r) = variableFree l && variableFree r
    variableFree (RelGEq l r) = variableFree l && variableFree r
    variableFree (RelLEq l r) = variableFree l && variableFree r
    variableFree (RelGt l r) = variableFree l && variableFree r
    variableFree (RelLt l r) = variableFree l && variableFree r
    variables (RelEq l r) = variables l <> variables r
    variables (RelNeq l r) = variables l <> variables r
    variables (RelGEq l r) = variables l <> variables r
    variables (RelLEq l r) = variables l <> variables r
    variables (RelGt l r) = variables l <> variables r
    variables (RelLt l r) = variables l <> variables r
    substitute f d (RelEq l r) = RelEq (substitute f d l) (substitute f d r)
    substitute f d (RelNeq l r) = RelNeq (substitute f d l) (substitute f d r)
    substitute f d (RelGEq l r) = RelGEq (substitute f d l) (substitute f d r)
    substitute f d (RelLEq l r) = RelLEq (substitute f d l) (substitute f d r)
    substitute f d (RelGt l r) = RelGt (substitute f d l) (substitute f d r)
    substitute f d (RelLt l r) = RelLt (substitute f d l) (substitute f d r)
    reduce (RelEq l r) = RelEq (reduce l) (reduce r)
    reduce (RelNeq l r) = RelNeq (reduce l) (reduce r)
    reduce (RelGEq l r) = RelGEq (reduce l) (reduce r)
    reduce (RelLEq l r) = RelLEq (reduce l) (reduce r)
    reduce (RelGt l r) = RelGt (reduce l) (reduce r)
    reduce (RelLt l r) = RelLt (reduce l) (reduce r)

instance Pretty Relation where
    pretty (RelEq a b) = pretty a <+> text "=" <+> pretty b
    pretty (RelNeq a b) = pretty a <+> text "!=" <+> pretty b
    pretty (RelGEq a b) = pretty a <+> text ">=" <+> pretty b
    pretty (RelLEq a b) = pretty a <+> text "<=" <+> pretty b
    pretty (RelGt a b) = pretty a <+> text ">" <+> pretty b
    pretty (RelLt a b) = pretty a <+> text "<" <+> pretty b

data Value
    = Variable Text
    | Constant Double
    | String Text
    | UnsafeUnquoted Text
    | Antiquoted String
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Expr Value where
    variableFree (Variable _) = False
    variableFree _ = True
    substitute f d (Variable f')
        | f == f' = Constant d
    substitute _ _ v = v
    reduce a = a
    variables (Variable v) = S.singleton v
    variables _ = S.empty

isIntegral :: RealFrac a => a -> Bool
isIntegral = (== 0) . snd @Integer . properFraction

instance Pretty Value where
    pretty (Variable t) = textStrict t
    pretty (Constant d)
        | isIntegral d =
            let d' :: Int
                d' = truncate d
             in pretty d'
        | otherwise = pretty d
    pretty (String t) = dquotes (textStrict t)
    pretty (UnsafeUnquoted t) = textStrict t
    pretty (Antiquoted x) = textStrict ("$" <> pack x)
