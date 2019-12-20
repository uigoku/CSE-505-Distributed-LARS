{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.LARS.AST where

import Control.Lens hiding (_head)
import Data.Expr
import Data.Set (Set)
import Data.Data
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Text.Lazy (toStrict)
import Data.List.Extra (nubOrd)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import GHC.Generics (Generic)
import Data.Generics.Multiplate
import Language.Haskell.TH.Lift

import qualified Data.Set as S
import qualified Data.Text as T

type Program = [Statement]

data Statement = StmtRule Rule | StmtShow Signature
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

allRules :: Program -> [Rule]
allRules xs = [ r | StmtRule r <- xs ]

showSigs :: Program -> [Signature]
showSigs xs = [ sig | StmtShow sig <- xs ]

instance Pretty Statement where
    pretty (StmtRule r) = pretty r
    pretty (StmtShow sig) = text "#show" <+> pretty sig <> dot

prettyProgram :: Program -> Text
prettyProgram = toStrict . displayT . renderPretty 0.95 80 . vcat . map pretty

data Rule =
    Rule Head
         [BodyElement]
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

ruleHead :: Rule -> Atom
ruleHead (Rule (Head h) _) = h

instance Pretty Rule where
    pretty (Rule h []) = pretty h <> dot
    pretty (Rule h bs) =
        pretty h <+>
        text ":-" <+> hcat (punctuate (char ',') (map pretty bs)) <> dot

data Head =
    Head Atom
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Pretty Head where
    pretty (Head a) = pretty a

data BodyElement
    = BodyAtom Atom
    | BodyRelation Relation
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Pretty BodyElement where
    pretty (BodyAtom a) = pretty a
    pretty (BodyRelation r) = pretty r

atomText :: BasicAtom -> Text
atomText = displayTStrict . renderOneLine . pretty

data BasicAtom = BasicAtom Bool Text [Value]
    deriving (Show, Read, Generic, Lift, Data, Typeable)

positive :: BasicAtom -> BasicAtom
positive (BasicAtom _ t vs) = BasicAtom True t vs

instance Eq BasicAtom where
    BasicAtom a b xs == BasicAtom c d ys =
        a == c && b == d && length xs == length ys

instance Ord BasicAtom where
    compare (BasicAtom a b xs) (BasicAtom c d ys) =
        compare (a, b, length xs) (c, d, length ys)

newtype GroundBasicAtom = GroundBasicAtom { unground :: BasicAtom }
    deriving (Show, Read, Generic, Lift, Data)

ground :: BasicAtom -> GroundBasicAtom
ground = GroundBasicAtom

instance Eq GroundBasicAtom where
    GroundBasicAtom (BasicAtom a b xs) 
      == GroundBasicAtom (BasicAtom c d ys) 
        = a == c && b == d && xs == ys

instance Ord GroundBasicAtom where
    compare (GroundBasicAtom (BasicAtom a b xs)) (GroundBasicAtom (BasicAtom c d ys)) =
        compare (a, b, xs) (c, d, ys)

instance Pretty GroundBasicAtom where
    pretty (GroundBasicAtom a) = pretty a

-- | Like 'tupled' but without the spaces
tupled' :: [Doc] -> Doc
tupled' = enclose lparen rparen . hcat . punctuate (char ',')

instance Pretty BasicAtom where
    pretty (BasicAtom f n []) = nin f <> textStrict n
    pretty (BasicAtom f n as) = nin f <> textStrict n <> tupled' (map pretty as)

data Signature = Signature Text Int
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

sigToAtom :: Signature -> BasicAtom
sigToAtom (Signature t i) =
    BasicAtom True t (map (Variable . pack . ('V' :) . show) [1 .. i])

instance Pretty Signature where
    pretty (Signature t a) = pretty t <> char '/' <> pretty a

data ExtendedAtom basic
    = Atom basic
    | AtAtom basic
             Value
    | AtWAtom basic
              Value
              Bool
              Window
    | AlwaysAtom basic
                 Bool
                 Window
    | HappenedAtom basic
                   Bool
                   Window
    deriving (Eq, Show, Ord, Read, Generic, Functor, Lift, Data, Typeable)

type Atom = ExtendedAtom BasicAtom

type GroundAtom = ExtendedAtom GroundBasicAtom

baseAtom :: ExtendedAtom basic -> basic
baseAtom a = case a of
    Atom b -> b
    AtAtom b _ -> b
    AtWAtom b _ _ _ -> b
    AlwaysAtom b _ _ -> b
    HappenedAtom b _ _ -> b

nin :: Bool -> Doc
nin False = text "not" <> space
nin _ = empty

instance Pretty basic => Pretty (ExtendedAtom basic) where
    pretty (Atom a) = pretty a
    pretty (AtAtom a v) = pretty a <+> "at" <+> pretty v
    pretty (AtWAtom a v n w) =
        pretty a <+> "at" <+> pretty v <+> nin n <> text "in" <+> pretty w
    pretty (AlwaysAtom a n w) =
        pretty a <+> text "always" <+> nin n <> text "in" <+> pretty w
    pretty (HappenedAtom a n w) = pretty a <+> nin n <> text "in" <+> pretty w

atomWindow :: Getter Atom (Maybe Window)
atomWindow = to go
  where
    go (AtWAtom _ _ _ w) = Just w
    go (AlwaysAtom _ _ w) = Just w
    go (HappenedAtom _ _ w) = Just w
    go _ = Nothing

data TimeUnit
    = Sec
    | Millisec
    | Minute
    | Hour
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

instance Pretty TimeUnit where
    pretty Sec = text "sec"
    pretty Minute = text "min"
    pretty Millisec = text "msec"
    pretty Hour = text "h"

data Window
    = SlidingTimeWindow TimeUnit
                        Value
    deriving (Eq, Show, Ord, Read, Generic, Lift, Data, Typeable)

toSec :: TimeUnit -> Int -> Int
toSec Sec x = x
toSec Minute x = x * 60
toSec Millisec x = x `div` 60
toSec Hour x = x * 60 * 60

windowSize :: Window -> Maybe NominalDiffTime
windowSize (SlidingTimeWindow unit (Constant x)) =
    Just . fromIntegral $ toSec unit (truncate x)
windowSize _ = Nothing

instance Pretty Window where
    pretty w =
        let u =
                case w of
                    SlidingTimeWindow x k ->
                        pretty k <+> pretty x
         in brackets u

makePrisms ''Statement

makePrisms ''Rule

makePrisms ''Head

makePrisms ''BodyElement

makePrisms ''Value

makePrisms ''ExtendedAtom

makePrisms ''Window

ruleBodyAtoms :: Rule -> [Atom]
ruleBodyAtoms = toListOf $ _Rule . _2 . traverse . _BodyAtom

allBodyAtoms :: Program -> Set Atom
allBodyAtoms = S.fromList . concatMap ruleBodyAtoms . allRules

extendedAtoms :: Program -> [Atom]
extendedAtoms rs = heads ++ bodies
  where
    heads = [h | Rule (Head h@(AtAtom _ _)) _ <- allRules rs]
    bodies = do
        Rule _ bs <- allRules rs
        BodyAtom b <- bs
        case b of
            Atom _ -> mempty
            _ -> pure b

-- | Find all (unground) basic atoms in a program which occur in a body, but not in a
-- head.
freeAtoms :: Program -> [BasicAtom]
freeAtoms rs = S.toList (bodies `S.difference` heads)
  where
    heads = S.fromList [positive (baseAtom h) | Rule (Head h) _ <- allRules rs]
    bodies =
        S.fromList $ do
            Rule _ bs <- allRules rs
            BodyAtom b <- bs
            pure . positive . baseAtom $ b

-- | Extract all basic atoms from a program /that occur in their basic form/.
basicAtoms :: Program -> [BasicAtom]
basicAtoms rs = heads ++ bodies
  where
    heads = [h | Rule (Head (Atom h)) _ <- allRules rs]
    bodies = [a | Rule _ bs <- allRules rs, BodyAtom (Atom a) <- bs ]

allBasicAtoms :: Program -> [BasicAtom]
allBasicAtoms p = nubOrd (map baseAtom (extendedAtoms p) <> basicAtoms p)

data LARSPlate f = LARSPlate
    { _stmt :: Statement -> f Statement
    , _rule :: Rule -> f Rule
    , _head :: Head -> f Head
    , _bodyElem :: BodyElement -> f BodyElement
    , _basicAtom :: BasicAtom -> f BasicAtom
    , _sig :: Signature -> f Signature
    , _extAtom :: ExtendedAtom BasicAtom -> f (ExtendedAtom BasicAtom)
    }

instance Multiplate LARSPlate where
    multiplate child =
        LARSPlate
            buildStmt
            buildRule
            buildHead
            buildBE
            buildBasicAtom
            buildSig
            buildExtAtom
      where
        buildStmt (StmtRule r) = StmtRule <$> _rule child r
        buildStmt (StmtShow s) = StmtShow <$> _sig child s
        buildRule (Rule h b) =
            Rule <$> _head child h <*> (traverse (_bodyElem child) b)
        buildHead (Head h) = Head <$> _extAtom child h
        buildBE (BodyAtom a) = BodyAtom <$> _extAtom child a
        buildBE x = pure x
        buildBasicAtom x = pure x
        buildSig x = pure x
        buildExtAtom (Atom b) = Atom <$> _basicAtom child b
        buildExtAtom (AtAtom b v) = AtAtom <$> _basicAtom child b <*> pure v
        buildExtAtom (AtWAtom b v x w) =
            AtWAtom <$> _basicAtom child b <*> pure v <*> pure x <*> pure w
        buildExtAtom (AlwaysAtom b x w) =
            AlwaysAtom <$> _basicAtom child b <*> pure x <*> pure w
        buildExtAtom (HappenedAtom b x w) =
            HappenedAtom <$> _basicAtom child b <*> pure x <*> pure w

    mkPlate build =
        LARSPlate
            (build _stmt)
            (build _rule)
            (build _head)
            (build _bodyElem)
            (build _basicAtom)
            (build _sig)
            (build _extAtom)

negPlate :: LARSPlate Identity
negPlate = purePlate {_basicAtom = goAtom, _sig = goSig}
  where
    goAtom (BasicAtom t n v) = pure $ BasicAtom t (go n) v
    goSig (Signature n v) = pure $ Signature (go n) v
    go t =
        case T.uncons t of
            Just ('-', t') -> "n_" <> t'
            _ -> t

-- | Renames negated atoms of the form @-a@ to @n_a@. This does not perform any
-- checks for collisions! This function exists primarily to accomodate Ticker's
-- parser.
renameNegation :: Program -> Program
renameNegation = map (runIdentity . _stmt (mapFamily negPlate))
