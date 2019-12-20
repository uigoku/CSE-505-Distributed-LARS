{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.EventTree
    ( Extended(..)
    , TimeInterval(..)
    , upperBounded
    , EventTree
    , insert
    , close
    , window
    , restrict
    -- * Queries
    , always
    , happened
    , atW
    , atT
    , latest
    ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Safe (lastMay)

import qualified Data.IntervalSet as I
import qualified Data.Map.Strict as M

data Extended a
    = Simple !a
    | Infinite
    deriving (Eq, Show, Read)

fromSimple :: Extended a -> a
fromSimple (Simple a) = a
fromSimple _ = error "Non-simple Extended found"

instance Ord a => Ord (Extended a) where
    _ <= Infinite = True
    Infinite <= _ = False
    Simple a <= Simple b = a <= b

-- | Half-open interval between 'UTCTime'. The interval is closed at the
-- start and open at the end.
data TimeInterval =
    TimeInterval !UTCTime
                 !(Extended UTCTime)
    deriving (Show, Eq, Ord)

instance I.Interval TimeInterval (Extended UTCTime) where
    lowerBound (TimeInterval b _) = Simple b
    upperBound (TimeInterval _ b) = b
    rightClosed _ = False

simpleLowerBound :: TimeInterval -> UTCTime
simpleLowerBound = fromSimple . I.lowerBound

upperBounded :: TimeInterval -> Bool
upperBounded (TimeInterval _ Infinite) = False
upperBounded _ = True

newtype EventTree event = EventTree
    { getEventTree :: M.Map event (I.IntervalSet TimeInterval)
    } deriving (Semigroup, Monoid, Show, Eq, Ord)

insert ::
       Ord event => UTCTime -> event -> EventTree event -> EventTree event
insert t e = EventTree . M.alter go e . getEventTree
  where
    go Nothing = Just $ I.singleton i
    go (Just s) =
        Just $
        if upperBounded . fromMaybe (error "SEVERE: Malformed event tree") $
           I.findLast s
            then I.insert i s
            else s
    i = TimeInterval t Infinite

close :: Ord event => UTCTime -> event -> EventTree event -> EventTree event
close t e = EventTree . M.adjust go e . getEventTree
  where
    go is =
        fromMaybe is $ do
            x@(TimeInterval start Infinite) <- I.findLast is
            pure . I.insert (TimeInterval start (Simple t)) . I.delete x $ is

-- | Merge time intervals to the extent possible. The input list is assumed to
-- be sorted (w.r.t. lower bounds)
merge :: [TimeInterval] -> [TimeInterval]
merge = foldr go []
  where
    go i [] = [i]
    go i (x:xs)
        | I.overlaps i x || I.upperBound i == I.lowerBound x =
            TimeInterval (simpleLowerBound i) (I.upperBound x) : xs
        | otherwise = i : x : xs

-- | Apply a time window and event query to an 'EventTree'. All intervals in
-- which the event holds will be contained in the resulting list, i.e. the
-- interval does not need to be /fully contained/ by the window.
window ::
       Ord event => TimeInterval -> event -> EventTree event -> [TimeInterval]
window i e =
    maybe [] (I.toAscList . flip I.intersecting i) . M.lookup e . getEventTree

-- | Restrict event tree to a window. Note that this operation scales linearly
-- in the number of ground atoms!
restrict :: Ord event => TimeInterval -> EventTree event -> EventTree event
restrict i = EventTree . M.map (flip I.intersecting i) . getEventTree

-- | Predicate for whether an event is /always/ true within a given time
-- interval.
always :: Ord event => TimeInterval -> event -> EventTree event -> Bool
always i e = maybe False (flip I.subsumes i) . lastMay . merge . window i e

-- | Predicate for whether an event holds at /some/ time in a given time
-- interval.
happened :: Ord event => TimeInterval -> event -> EventTree event -> Bool
happened i e = not . null . window i e

-- | Find all timestamps at which a given event /became true/ within a given
-- time interval.
atW :: Ord event => TimeInterval -> event -> EventTree event -> [UTCTime]
atW i e = filter (simpleLowerBound i <=) . map simpleLowerBound . window i e

-- | Predicate for whether an event holds at a given timestamp.
atT :: Ord event => UTCTime -> event -> EventTree event -> Bool
atT t e =
    maybe False (not . null . flip I.containing (Simple t)) .
    M.lookup e . getEventTree

-- | Return the latest interval (by endpoint) for a given event.
latest :: Ord event => event -> EventTree event -> Maybe TimeInterval
latest e = I.findLast <=< M.lookup e . getEventTree
