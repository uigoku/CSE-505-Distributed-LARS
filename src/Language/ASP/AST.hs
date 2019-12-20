-- | Simplified ASP syntax tree
{-# LANGUAGE OverloadedStrings #-}
module Language.ASP.AST where

import Data.Expr
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text

type Program = [Rule]

prettyProgram :: Program -> Text
prettyProgram = displayTStrict . renderPretty 0.95 80 . vcat . map pretty

data Rule =
    Rule Atom
         [BodyElement]
    deriving (Eq, Show, Ord, Read)

instance Pretty Rule where
    pretty (Rule h []) = pretty h <> dot
    pretty (Rule h bs) =
        pretty h <+>
        text ":-" <+> hcat (punctuate (char ',') (map pretty bs)) <> dot

data BodyElement
    = BodyAtom Atom
    | BodyRelation Relation
    | BodyInterval Value
                   Value
                   Value
    deriving (Eq, Show, Ord, Read)

instance Pretty BodyElement where
    pretty (BodyAtom a) = pretty a
    pretty (BodyRelation r) = pretty r
    pretty (BodyInterval x a b) =
        pretty x <+> equals <+> pretty a <> text ".." <> pretty b

data Atom
    = Atom Text
           [Value]
    | Negated Atom
    deriving (Eq, Show, Ord, Read)

instance Pretty Atom where
    pretty (Negated a) = text "not" <+> pretty a
    pretty (Atom n []) = textStrict n
    pretty (Atom n vs) = textStrict n <> tupled (map pretty vs)

renamedAtom :: Text -> Atom -> Atom
renamedAtom t (Negated a) = Negated $ renamedAtom t a
renamedAtom t (Atom _ vs) = Atom t vs

extended :: [Value] -> Atom -> Atom
extended v (Negated a) = Negated $ extended v a
extended v (Atom n vs) = Atom n (vs <> v)

data Signature = Signature Text Int
    deriving (Eq, Show, Ord, Read)
