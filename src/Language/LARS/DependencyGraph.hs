{-# LANGUAGE OverloadedStrings #-}

module Language.LARS.DependencyGraph
    ( dGraph
    , componentGraph
    , singleNodeGraph
    , dotGraph
    , Dependency(..)
    , stratified
    , linearize
    , DependencyGraph
    , ComponentGraph
    , sourceNodes
    , sinkNodes
    , want
    , dependencies
    , NetworkNode(..)
    , nodeProgram
    ) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.List.Extra (nubOrd)
import Data.Bifunctor
import Data.Functor.Identity
import Data.Functor.Const
import Data.Maybe
import Data.Text (unpack)
import Language.LARS.AST
import Text.PrettyPrint.Leijen.Text
       (Pretty(..), char, displayTStrict, renderPretty, renderCompact, text, vcat)

import qualified Data.Set as S
import qualified Data.IntMap.Lazy as IM

data Dependency
    = SameTime
    | After
    | AfterOrSame
    deriving (Eq, Show, Ord, Read)

instance Pretty Dependency where
    pretty SameTime = char '='
    pretty After = char '>'
    pretty AfterOrSame = text ">="

symmetry :: (a, a, b) -> [(a, a, b)]
symmetry (a, b, c) = [(a, b, c), (b, a, c)]

dependencies :: Program -> [(Atom, Atom, Dependency)]
dependencies p =
    concatMap same (allRules p) <> concatMap symmetry (mapMaybe equiv aplus) <>
    mapMaybe gt aplus <>
    negated
  where
    aplus = extendedAtoms p
    same (Rule (Head h) bs) = [(h, b, AfterOrSame) | BodyAtom b <- bs]
    equiv a =
        case a of
            AtWAtom b v _ _ -> Just $ (AtAtom b v, Atom b, SameTime)
            AtAtom b _ -> Just $ (a, Atom b, SameTime)
            _ -> Nothing
    gt a =
        let b =
                case a of
                    AtWAtom x _ _ _ -> Just . Atom $ x
                    AlwaysAtom x _ _ -> Just . Atom $ x
                    HappenedAtom x _ _ -> Just . Atom $ x
                    _ -> Nothing
        in (\x -> (a, x, After)) <$> b
    negated =
        [ (Atom (BasicAtom False n vs), Atom (BasicAtom True n vs), AfterOrSame)
        | BasicAtom False n vs <- basicAtoms p
        ]

type DependencyGraph = Gr Atom Dependency

dGraph :: Program -> (DependencyGraph, NodeMap Atom)
dGraph p =
    let ds = nubOrd $ dependencies p
        as = S.toList $ foldr (\(a, b, _) -> S.insert a . S.insert b) S.empty ds
     in mkMapGraph as ds

dotGraph :: (Pretty a, Pretty b) => Gr a b -> String
dotGraph =
    showDot .
    fglToDotString .
    nemap
        (unpack . displayTStrict . renderPretty 1.0 80 . pretty)
        (unpack . displayTStrict . renderCompact . pretty)

-- | A dependency graph is stratified (stratifiable) iff a component graph
-- exists.
stratified :: DependencyGraph -> Bool
stratified = isJust . componentGraph []

data NetworkNode f = 
    Reasoner 
        { reasonerProgram :: Program
        , publishes :: f (S.Set BasicAtom)
        }
    | MasterNode
        { publishes :: f (S.Set BasicAtom)
        }

instance Pretty (NetworkNode f) where
    pretty (Reasoner p _) = vcat . map pretty $ p
    pretty (MasterNode _) = text "<<master>>"

nodeProgram :: NetworkNode f -> Program
nodeProgram (Reasoner p _) = p
nodeProgram _ = []

type ComponentGraph = Gr (NetworkNode Identity) [BasicAtom]

masterNode :: Node
masterNode = 0

-- | Calculate the component graph of a program, given its dependency graph
componentGraph :: Program -> DependencyGraph -> Maybe ComponentGraph
componentGraph p g =
    let after as bs =
            any (\(x, y) -> hasLEdge g (x, y, After)) ((,) <$> as <*> bs)
        -- The nodes of the component graph are the connected components of the
        -- dependency graph
        ns :: [(Int, ([Node], [Rule]))]
        ns =
            map (second (\x -> (x, atomRules (allRules p) . mapMaybe (lab g) $ x))) .
            zip [succ masterNode ..] . components . elfilter (/= After) $
            g
        -- An edge (a,b) is in the component graph if and only if there are
        -- nodes a',b' in a,b respectively, such that a' > b' is an edge in the
        -- dependency graph.
        -- Nodes are labelled with the rules deriving the respective atoms in
        -- the component. 
        -- An edge (a,b) is labelled with the set of atoms
        -- derived in a that are used in b.
        es :: [LEdge Edge]
        es =
            [ (b, a, (b, a))
            | (a, (a', _)) <- ns
            , (b, (b', _)) <- ns
            , after a' b'
            ]
        g' =
            nmap (\x -> Reasoner (map StmtRule x) (Const ())) .
            labfilter (not . null) . nmap snd $
            mkGraph ns es
        -- g' but with master node inserted
        g'' = forward' p g'
     in if hasLoop (tc g')
            then Nothing
            else Just g''

forward' ::
       Program -> Gr (NetworkNode (Const ())) (Node, Node) -> ComponentGraph
forward' p g =
    forward p .
    insEdges
        ([(masterNode, s, (masterNode, s)) | s <- sourceNodes g] <>
         [(s, masterNode, (s, masterNode)) | s <- sinkNodes g]) .
    insNode (masterNode, MasterNode (Const ())) $
    g

singleNodeGraph :: Program -> ComponentGraph
singleNodeGraph p =
    let g = mkGraph [(1, Reasoner (map StmtRule $ allRules p) (Const ()))] []
     in forward' p g

-- | The source nodes of a component graph are all nodes that do not have
-- predecessors.
sourceNodes :: Gr a b -> [Node]
sourceNodes g =
    let g' = delNode masterNode g
     in nodes . nfilter (null . pre g) $ g'

-- | The sink nodes of a component graph are all nodes that do not have
-- successors.
sinkNodes :: Gr a b -> [Node]
sinkNodes g =
    let g' = delNode masterNode g
     in nodes . nfilter (null . suc g') $ g'

-- | The atoms a program produces, i.e. derives. Equivalent to /locally
-- intrinsic/ atoms.
produces :: Program -> S.Set BasicAtom
produces p =
    S.fromList (map positive $ basicAtoms p) `S.difference`
    S.fromList (freeAtoms p)

-- | The want set of a program, i.e. the atoms it uses but does not produce.
want :: Program -> S.Set BasicAtom
want = S.fromList . freeAtoms

nimap :: DynGraph gr => (Node -> a -> c) -> gr a b -> gr c b
nimap f = gmap go
  where
    go (i, u, l, o) = (i, u, f u l, o)

forward ::
       Program
    -> Gr (NetworkNode f) Edge
    -> Gr (NetworkNode Identity) [BasicAtom]
forward fullProgram g = nimap labelNode . emap (S.toList . uncurry listening) $ g
  where
    publishing :: IM.IntMap (S.Set BasicAtom)
    publishing =
        IM.fromList $ do
            u <- nodes g
            case lab g u of
                Just (Reasoner _ _) ->
                    let succAugWants = S.unions [augwant v | v <- suc g u]
                     in pure $ (u, succAugWants)
                Just (MasterNode _) -> pure (u, want fullProgram)
                Nothing -> error "Invalid node"

    augwant :: Node -> S.Set BasicAtom
    augwant u =
        case lab g u of
            Just (Reasoner progU _) ->
                fromMaybe S.empty $ do
                    pubU <- u `IM.lookup` publishing
                    pure $
                        want progU `S.union`
                        (pubU `S.difference` produces progU)
            Just (MasterNode _) -> 
                    case showSigs fullProgram of
                        [] -> produces fullProgram
                        xs -> S.fromList $ map sigToAtom xs
            Nothing -> error "Invalid node"

    listening :: Node -> Node -> S.Set BasicAtom
    listening v u =
        fromMaybe S.empty $ do
            pubV <- v `IM.lookup` publishing
            pure $ pubV `S.intersection` augwant u

    labelNode :: Node -> NetworkNode f -> NetworkNode Identity
    labelNode u x =
        x {publishes = Identity $ fromMaybe S.empty (u `IM.lookup` publishing)}

-- | Filter a list of rules through a list of atoms such that only rules
-- deriving the specified atoms remain.
atomRules :: [Rule] -> [Atom] -> [Rule]
atomRules p xs =
    let xs' = S.fromList xs
    in filter (\(Rule (Head h) _) -> h `elem` xs') p

-- | Linearize a LARS program.
linearize :: Program -> Maybe [Program]
linearize p =
    map nodeProgram . topsort' <$> componentGraph p (fst $ dGraph p)
