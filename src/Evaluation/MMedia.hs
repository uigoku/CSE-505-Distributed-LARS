{-# LANGUAGE QuasiQuotes #-}

module Evaluation.MMedia
    ( mmedia
    , mmediaTicker
    ) where

import Data.Expr
import Data.List.NonEmpty (NonEmpty)
import Evaluation.Types
import Language.LARS.TH
import Reasoner.Tickless (WireAtom(..))
import System.Random

import qualified Language.LARS as LARS

-- | Generator for MMedia programs of a given window size.
mmediaEnc :: Int -> LARS.Program
mmediaEnc x =
    [lars|
    high :- value(V), alpha(V) [$w sec], 18 <= V.
    mid :- value(V), alpha(V) [$w sec], 12 <= V, V < 18.
    low :- value(V), alpha(V) [$w sec], V <= 12.
    lfu :- high always [$w sec].
    lru :- mid always [$w sec].
    fifo :- low always [$w sec], rtm50 [$w sec].
    done :- lfu.
    done :- lru.
    done :- fifo.
    random :- not done.
    value(5).
    value(15).
    value(25).
    finish :- off [1 sec], done.
    finish :- off [1 sec], random.
    |]
  where
    w = Constant (fromIntegral x)

mmediaEncTicker :: Int -> LARS.Program
mmediaEncTicker x =
    [lars|
    high at T :- value(V), alpha(V) at T [$w sec], 18 <= V.
    mid at T :- value(V), alpha(V) at T [$w sec], 12 <= V, V < 18.
    low at T :- value(V), alpha(V) at T [$w sec], V <= 12.
    lfu :- high always [$w sec].
    lru :- mid always [$w sec].
    fifo :- low always [$w sec], rtm50 [$w sec].
    done :- lfu.
    done :- lru.
    done :- fifo.
    random :- not done.
    value(5).
    value(15).
    value(25).
    finish :- off [1 sec], done.
    finish :- off [1 sec], random.
    |]
  where
    w = Constant (fromIntegral x)

mmediaDet :: [NonEmpty WireAtom]
mmediaDet = continuous $ map (mmediaAlpha . (`rem` 180)) [0 ..]

mmediaAlpha :: Int -> LARS.GroundBasicAtom
mmediaAlpha q
    | 0 <= q && q < 60 = [gatom|alpha(5)|]
    | q >= 60 && q < 120 = [gatom|alpha(15)|]
    | otherwise = [gatom|alpha(25)|]

-- | "Non-deterministic" instance given a seed.
mmediaNonDet :: Int -> [NonEmpty WireAtom]
mmediaNonDet = continuous . map (mmediaAlpha) . randomRs (0, 360) . mkStdGen

mmedia :: Int -> Maybe Int -> Evaluation
mmedia w rng =
    Evaluation
        (mmediaEnc w)
        (maybe mmediaDet mmediaNonDet rng)
        ([gatom|off|])
        ([gatom|finish|])
        Nothing

mmediaTicker :: Int -> Maybe Int -> Evaluation
mmediaTicker w rng =
    Evaluation
        (mmediaEncTicker w)
        (maybe mmediaDet mmediaNonDet rng)
        ([gatom|off|])
        ([gatom|finish|])
        Nothing
