{-# LANGUAGE OverloadedLists #-}

module Evaluation.Types
    ( Evaluation(..)
    , limit
    , continuous
    , scanLog
    ) where

import Data.Time (DiffTime)
import Data.List.NonEmpty (NonEmpty)
import Reasoner.Tickless (PhaseChange(..), WireAtom(..))
import qualified Language.LARS as LARS
import qualified Data.Set as S

data Evaluation =
    Evaluation LARS.Program
               [NonEmpty WireAtom]
               LARS.GroundBasicAtom
               LARS.GroundBasicAtom
               (Maybe DiffTime)

limit :: Int -> Evaluation -> Evaluation
limit n (Evaluation p i o f dt) =
    Evaluation p (take n i <> [[WireAtom Positive o]]) o f dt

continuous :: [LARS.GroundBasicAtom] -> [NonEmpty WireAtom]
continuous [] = []
continuous xs@(x:_) =
    [WireAtom Positive x] :
    zipWith
        (\e s ->
             if e /= s
                 then [WireAtom Negative e, WireAtom Positive s]
                 else [WireAtom Positive s])
        xs
        (tail xs)

scanLog :: [NonEmpty WireAtom] -> [[LARS.GroundBasicAtom]]
scanLog = map S.toList . scanr apply S.empty
  where
    apply changes current =
        let change s (WireAtom Negative a) = S.delete a s
            change s (WireAtom Positive a) = S.insert a s
         in foldl change current changes
