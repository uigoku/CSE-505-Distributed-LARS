{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Evaluation.NQueens (
    nqueens
) where

import Data.Expr
import Data.Generics.Multiplate
import Data.Functor.Identity
import Evaluation.Types
import Data.List.NonEmpty (NonEmpty)
import Language.LARS.TH
import Reasoner.Tickless (WireAtom(..))
import Text.Read (readMaybe)

import qualified Language.LARS as LARS
import qualified Data.Text as T

nqueens :: Int -> Int -> Evaluation
nqueens n pipelineStages =
    Evaluation
        (nqueensEnc n pipelineStages)
        (nqueensDet n)
        ([gatom|off|])
        ([gatom|finish|])
        (Just 0.05)

nqueensBase :: Int -> LARS.Program
nqueensBase n' =
    [lars|
    q0(X,Y) :- d0(Y),d0(X),not -q0(X,Y).
    -q0(X,Y) :- d0(Y),d0(X),not q0(X,Y).

    aux0 :- q0(X1,Y),q0(X2,Y),X1 < X2,not aux0.
    aux0 :- q0(X,Y1),q0(X,Y2),Y1 < Y2,not aux0.
    aux0 :- q0(X1,Y1), q0(X2,Y2), X1<X2, Y1<Y2, d0(Z1), d0(Z2), Z1 = X2-X1, Z2 = Y2-Y1, Z1 = Z2,not aux0.
    aux0 :- q0(X1,Y1), q0(X2,Y2), X1<X2, Y2<Y1, d0(Z1), d0(Z2), Z1 = X2-X1, Z2 = Y1-Y2, Z1 = Z2,not aux0.

    placed0(X) :- q0(X,Y),d0(Y).
    aux0 :- not placed0(X),d0(X),not aux0.

    send0(Y) :- q0($n,Y).
    #show q0/1.
    |]
  where
    n = Constant $ fromIntegral n'

nqueensDomain :: Int -> LARS.Program
nqueensDomain n =
    concat [[lars|d0($x).|] | x <- map (Constant . fromIntegral) [1 .. n]]

nqueensTrigger :: LARS.Program
nqueensTrigger = [lars|q1(1,Y) :- send0(Y) [50 msec], d1(Y).|]

addPredPlate :: Int -> LARS.LARSPlate Identity
addPredPlate y = purePlate {LARS._basicAtom = goAtom, LARS._sig = goSig}
  where
    goAtom (LARS.BasicAtom t n v) = pure $ LARS.BasicAtom t (incr n) v
    goSig (LARS.Signature n v) = pure $ LARS.Signature (incr n) v
    incr t =
        case readMaybe . pure . T.last $ t of
            Just (x :: Int) -> T.init t <> T.pack (show $ x + y)
            Nothing -> t

addPred :: Int -> LARS.Program -> LARS.Program
addPred n = map (runIdentity . LARS._stmt (mapFamily $ addPredPlate n))

nqueensStage :: Int -> Int -> LARS.Program
nqueensStage nums stage =
    addPred stage (nqueensBase stage) <> addPred (pred stage) nqueensTrigger <>
    addPred stage (nqueensDomain nums)

finish :: Int -> LARS.Program
finish finalStage = addPred finalStage 
    [lars|
    finish :- off [180 sec], q0(X,Y), d0(X), d0(Y).
    #show finish/0.
    |]

nqueensEnc :: Int -> Int -> LARS.Program
nqueensEnc n stages =
    concat [nqueensStage n stage | stage <- [1 .. stages]] <> finish stages

nqueensDet :: Int -> [NonEmpty WireAtom]
nqueensDet n = continuous $ map go [x `rem` n + 1 | x <- [0 ..]]
  where
    go x =
        let y = Constant (fromIntegral x)
         in [gatom|send0($y)|] 
