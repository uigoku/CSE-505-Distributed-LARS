{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reasoner.Tickless.Database.Selda
    ( DBAtom
    , atomTable
    -- * Queries
    , always
    , happened
    , atW
    -- * Insert
    , events
    ) where

import Data.Expr (Value)
import Data.Maybe (mapMaybe)
import Data.String
import Data.Time
import Data.Typeable hiding (cast)
import Database.Selda
import Database.Selda.SqlType
import Database.Selda.Unsafe
import Text.PrettyPrint.Leijen.Text (displayTStrict, pretty, renderOneLine)

import qualified Data.Text as T
import qualified Language.LARS as LARS

newtype AtomParams =
    AtomParams [Text]
    deriving (Generic, Typeable)

params :: [Value] -> AtomParams
params = AtomParams . map (displayTStrict . renderOneLine . pretty)

instance SqlType AtomParams where
    mkLit (AtomParams ps) = LCustom $ LText (T.intercalate "," ps)
    sqlType _ = TText
    fromSql (SqlString t) = AtomParams $ T.splitOn "," t
    fromSql _ = error "fromSql: Invalid AtomParams"
    defaultValue = mkLit $ AtomParams []

data DBPhase
    = Positive
    | Negative
    | Retrigger
    deriving (Generic, Bounded, Enum, Show, Read, SqlType)

data DBAtom = DBAtom
    { dbAtomID :: ID DBAtom
    , dbAtomName :: Text
    , dbAtomParams :: AtomParams
    , dbAtomTime :: UTCTime
    , dbAtomRun :: Int
    , dbAtomPhase :: DBPhase
    } deriving (Generic, SqlRow)

-- | Log a new event in the database
events :: MonadSelda m => UTCTime -> Int -> [LARS.BasicAtom] -> m ()
events _ _ [] = pure ()
events now run as@(a:_) = do
    tryCreateTable (atomTable a)
    insert_ (atomTable a) (mapMaybe toDB as)
  where
    toDB (LARS.BasicAtom True n vs) =
        Just $ DBAtom def n (params vs) now run Positive
    toDB _ = Nothing

-- | Table descriptor for a given atom.
atomTable :: LARS.BasicAtom -> Table DBAtom
atomTable (LARS.BasicAtom _ n _) =
    table (fromString . T.unpack $ n) [#dbAtomID :- autoPrimary]

-- | Select the entire past of an atom
selectPastOf :: LARS.BasicAtom -> Query s (Row s DBAtom)
selectPastOf atom@(LARS.BasicAtom _ _ ps) = do
    xs <- select (atomTable atom)
    restrict (xs ! #dbAtomParams .== literal (params ps))
    pure xs

always :: UTCTime -> LARS.BasicAtom -> LARS.Window -> Query s (Col s Bool)
always now a w = (.&&) <$> wasMadeTrue <*> (not_ <$> falsifiedInWindow)
  where
    wasMadeTrue = do
        l <-
            aggregate $ do
                occ <- selectPastOf a
                beforeWindow now (atomTable a) occ w
                pure (max_ (occ ! #dbAtomTime))
        occ <- selectPastOf a
        restrict $ just (occ ! #dbAtomTime) .== l
        pure $
            occ ! #dbAtomPhase .== literal Positive 
            .|| occ ! #dbAtomPhase .== literal Retrigger
    falsifiedInWindow = do
        c <-
            aggregate $ do
                occ <- selectPastOf a
                inWindow now (atomTable a) occ w
                restrict (occ ! #dbAtomPhase .== literal Negative)
                pure (count (occ ! #dbAtomID))
        pure $ c .> 0

happened :: UTCTime -> LARS.BasicAtom -> LARS.Window -> Query s (Col s Bool)
happened now a w = do
    x <-
        aggregate $ do
            occ <- selectPastOf a
            restrict $
                (occ ! #dbAtomPhase .== literal Positive) .||
                (occ ! #dbAtomPhase .== literal Retrigger)
            inWindow now (atomTable a) occ w
            pure (count $ occ ! #dbAtomID)
    pure $ x .>= 1

atW :: UTCTime -> LARS.BasicAtom -> LARS.Window -> Query s (Col s UTCTime)
atW now a w = do
    occ <- selectPastOf a
    inWindow now (atomTable a) occ w
    restrict $
        (occ ! #dbAtomPhase .== literal Positive) .||
        (occ ! #dbAtomPhase .== literal Retrigger)
    pure $ occ ! #dbAtomTime

-- | Filter rows in a given table through a given window.
inWindow ::
       forall s.
       UTCTime
    -> Table DBAtom
    -> Row s DBAtom
    -> LARS.Window
    -> Query s ()
inWindow now _ x (LARS.SlidingTimeWindow n a) =
    restrict
        (x ! #dbAtomTime .>=
         literal ((negate . fromIntegral . LARS.toSec n $ a) `addUTCTime` now))
inWindow _ t x (LARS.SlidingTupleWindow n) = do
    latest <-
        aggregate $ do
            tuples <- select t
            pure $ max_ (tuples ! #dbAtomID)
    let startTuple :: Col s Int
        startTuple = matchNull 0 (subtract $ literal n) (cast latest)
    restrict (cast (x ! #dbAtomID) .>= startTuple)

beforeWindow ::
    forall s.
       UTCTime
    -> Table DBAtom
    -> Row s DBAtom
    -> LARS.Window
    -> Query s ()
beforeWindow now _ x (LARS.SlidingTimeWindow n a) =
    restrict
        (x ! #dbAtomTime .<=
         literal ((negate . fromIntegral . LARS.toSec n $ a) `addUTCTime` now))
beforeWindow _ t x (LARS.SlidingTupleWindow n) = do
    latest <-
        aggregate $ do
            tuples <- select t
            pure $ max_ (tuples ! #dbAtomID)
    let startTuple :: Col s Int
        startTuple = matchNull 0 (subtract $ literal n) (cast latest)
    restrict (cast (x ! #dbAtomID) .<= startTuple)

instance (MonadMask m, MonadSelda m) => MonadSelda (ExceptT e m) where
    seldaConnection = lift seldaConnection
