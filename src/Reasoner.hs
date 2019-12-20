module Reasoner (
    reasoner,
    Reasoner(..),
    External.ExternalReasoner(..),
    Ticker.TickerReasoner(..)
) where

import Data.Text (Text)
import Data.Set (Set)
import Language.LARS (BasicAtom, Program)

import qualified Reasoner.Tickless as Tickless
import qualified Reasoner.External as External
import qualified Reasoner.External.Ticker as Ticker

data Reasoner = Tickless | External External.ExternalReasoner

reasoner ::
       Reasoner
    -> Text
    -> Program
    -> [String]
    -> String
    -> Set BasicAtom
    -> Set BasicAtom
    -> IO ()
reasoner Tickless = Tickless.reasoner
reasoner (External e) = External.reasoner e
