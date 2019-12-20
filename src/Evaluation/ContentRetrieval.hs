{-# LANGUAGE QuasiQuotes #-}

module Evaluation.ContentRetrieval
    ( contentRetrieval
    ) where

import Data.Expr
import Evaluation.Types
import Language.LARS.TH

import qualified Language.LARS as LARS

-- | Generator for Content Retrieval programs of a given window size.
contentRetrievalEnc :: Int -> LARS.Program
contentRetrievalEnc x =
    [lars|
    need(I,N) :- item(I), node(N), req(I,N) in [$w s].
    avail(I,N) :- item(I), node(N), cache(I,N) in [$w s].
    get(I,N,M) :- source(I,N,M), not nGet(I,N,M).
    nGet(I,N,M) :- node(M), get(I,N,M'), M != M'.
    nGet(I,N,M) :- source(I,N,M), source(I,N,M'), M != M', 
        qual(M,L), qual(M',L'), L < L'.
    source(I,N,M) :- need(I,N), not avail(I,N), avail(I,M), reach(N,M).
    reach(N,M) :- conn(N,M).
    reach(N,M) :- reach(N,M'), conn(M',M), M' != M, N != M.
    conn(N,M) :- edge(N,M), not down(M) always [$w s].
    qual(N,L) :- node(N), lev(L), lev(L'), L' < L, 
        qLev(N,L) in [$w s], not qLev(N,L') in [$w s].
    finish :- off [1 sec].
    |]
  where
    w = Constant (fromIntegral x)

contentRetrieval :: Int -> Evaluation
contentRetrieval w =
    Evaluation (contentRetrievalEnc w) [] ([gatom|off|]) ([gatom|finish|]) Nothing
