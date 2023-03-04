{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ECTA.Internal.ECTA.Enumeration (
    TermFragment(..)
  , termFragToTruncatedTerm

  , SuspendedConstraint(..)
  , scGetPathTrie
  , scGetUVar
  , descendScs
  , UVarValue(..)

  , EnumerationState(..)
  , uvarCounter
  , uvarRepresentative
  , uvarValues
  , initEnumerationState


  , EnumerateM
  , getUVarRepresentative
  , assimilateUvarVal
  , mergeNodeIntoUVarVal
  , getUVarValue
  , getTermFragForUVar
  , runEnumerateM


  , enumerateNode
  , enumerateEdge
  , firstExpandableUVar
  , enumerateOutUVar
  , enumerateOutFirstExpandableUVar
  , enumerateFully
  , expandTermFrag
  , expandUVar
  , sampleTerm

  , getAllTruncatedTerms
  , getAllTerms
  , naiveDenotation
  , naiveDenotationBounded
  ) where

import Control.Monad ( forM_, guard )
import Control.Monad.State.Strict ( StateT(..) )
import qualified Data.IntMap as IntMap
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe, fromJust, isJust )
import Data.Monoid ( Any(..) )
import Data.Semigroup ( Max(..) )
import           Data.Sequence ( Seq((:<|), (:|>)) )
import qualified Data.Sequence as Sequence
import Control.Monad.Identity ( Identity )
import System.Random ( mkStdGen, StdGen, uniformR )

import Control.Lens ( use, ix, (%=), (.=) )
import Control.Lens.TH ( makeLenses )
import           Pipes
import qualified Pipes.Prelude as Pipes

import Data.List.Index ( imapM )

import Data.ECTA.Internal.ECTA.Operations
import Data.ECTA.Internal.ECTA.Type
import Data.ECTA.Paths
import Data.ECTA.Term
import           Data.Persistent.UnionFind ( UnionFind, UVar, uvarToInt, intToUVar, UVarGen )
import qualified Data.Persistent.UnionFind as UnionFind
import Data.Text.Extended.Pretty

-------------------------------------------------------------------------------


---------------------------------------------------------------------------
------------------------------- Term fragments ----------------------------
---------------------------------------------------------------------------

data TermFragment = TermFragmentNode !Symbol ![TermFragment]
                  | TermFragmentUVar UVar
  deriving ( Eq, Ord, Show )

termFragToTruncatedTerm :: TermFragment -> Term
termFragToTruncatedTerm (TermFragmentNode s ts) = Term s (map termFragToTruncatedTerm ts)
termFragToTruncatedTerm (TermFragmentUVar uv)   = Term (Symbol $ "v" <> pretty (uvarToInt uv)) []

---------------------------------------------------------------------------
------------------------------ Enumeration state --------------------------
---------------------------------------------------------------------------

-----------------------
------- Suspended constraints
-----------------------

data SuspendedConstraint = SuspendedConstraint !PathTrie !UVar
  deriving ( Eq, Ord, Show )

scGetPathTrie :: SuspendedConstraint -> PathTrie
scGetPathTrie (SuspendedConstraint pt _) = pt

scGetUVar :: SuspendedConstraint -> UVar
scGetUVar (SuspendedConstraint _ uv) = uv

descendScs :: Int -> Seq SuspendedConstraint -> Seq SuspendedConstraint
descendScs i scs = Sequence.filter (not . isEmptyPathTrie . scGetPathTrie)
                   $ fmap (\(SuspendedConstraint pt uv) -> SuspendedConstraint (pathTrieDescend pt i) uv)
                          scs

-----------------------
------- UVarValue
-----------------------

data UVarValue = UVarUnenumerated { contents    :: !(Maybe Node)
                                  , constraints :: !(Seq SuspendedConstraint)
                                  }
               | UVarEnumerated   { termFragment :: !TermFragment }
               | UVarEliminated
  deriving ( Eq, Ord, Show )

intersectUVarValue :: UVarValue -> UVarValue -> UVarValue
intersectUVarValue (UVarUnenumerated mn1 scs1) (UVarUnenumerated mn2 scs2) =
  let newContents = case (mn1, mn2) of
                      (Nothing, x      ) -> x
                      (x,       Nothing) -> x
                      (Just n1, Just n2) -> Just (intersect n1 n2)
      newConstraints = scs1 <> scs2
  in UVarUnenumerated newContents newConstraints

intersectUVarValue UVarEliminated            _                         = error "intersectUVarValue: Unexpected UVarEliminated"
intersectUVarValue _                         UVarEliminated            = error "intersectUVarValue: Unexpected UVarEliminated"
intersectUVarValue _                         _                         = error "intersectUVarValue: Intersecting with enumerated value not implemented"


-----------------------
------- Top-level state
-----------------------

instance Ord StdGen where
  (<=) s1 s2 = show s1 <= show s2

data EnumerationState = EnumerationState {
    _uvarCounter        :: UVarGen
  , _uvarRepresentative :: UnionFind
  , _uvarValues         :: Seq UVarValue
  , _randomSeed         :: Maybe StdGen
  }
  deriving ( Eq, Ord, Show )

makeLenses ''EnumerationState

initEnumerationState :: Maybe Int -> Node -> EnumerationState
initEnumerationState seed n = let (uvg, uv) = UnionFind.nextUVar UnionFind.initUVarGen
                                  rseed = fmap mkStdGen seed
                               in EnumerationState uvg
                                                   (UnionFind.withInitialValues [uv])
                                                   (Sequence.singleton (UVarUnenumerated (Just n) Sequence.Empty))
                                                   rseed



---------------------------------------------------------------------------
---------------------------- Enumeration monad ----------------------------
---------------------------------------------------------------------------

---------------------
-------- Monad
---------------------


type EnumerateM = StateT EnumerationState []

runEnumerateM :: EnumerateM a -> EnumerationState -> [(a, EnumerationState)]
runEnumerateM = runStateT


---------------------
-------- UVar accessors
---------------------

nextUVar :: EnumerateM UVar
nextUVar = do c <- use uvarCounter
              let (c', uv) = UnionFind.nextUVar c
              uvarCounter .= c'
              return uv

addUVarValue :: Maybe Node -> EnumerateM UVar
addUVarValue x = do uv <- nextUVar
                    uvarValues %= (:|> (UVarUnenumerated x Sequence.Empty))
                    return uv

getUVarValue :: UVar -> EnumerateM UVarValue
getUVarValue uv = do uv' <- getUVarRepresentative uv
                     let idx = uvarToInt uv'
                     values <- use uvarValues
                     return $ Sequence.index values idx

getTermFragForUVar :: UVar -> EnumerateM TermFragment
getTermFragForUVar uv =  termFragment <$> getUVarValue uv

getUVarRepresentative :: UVar -> EnumerateM UVar
getUVarRepresentative uv = do uf <- use uvarRepresentative
                              let (uv', uf') = UnionFind.find uv uf
                              uvarRepresentative .= uf'
                              return uv'

---------------------
-------- Creating UVar's
---------------------

pecToSuspendedConstraint :: PathEClass -> EnumerateM SuspendedConstraint
pecToSuspendedConstraint pec = do uv <- addUVarValue Nothing
                                  return $ SuspendedConstraint (getPathTrie pec) uv


---------------------
-------- Merging UVar's / nodes
---------------------

assimilateUvarVal :: UVar -> UVar -> EnumerateM ()
assimilateUvarVal uvTarg uvSrc
                                | uvTarg == uvSrc      = return ()
                                | otherwise            = do
  values <- use uvarValues
  let srcVal  = Sequence.index values (uvarToInt uvSrc)
  let targVal = Sequence.index values (uvarToInt uvTarg)
  case srcVal of
    UVarEliminated -> return () -- Happens from duplicate constraints
    _              -> do
      let v = intersectUVarValue srcVal targVal
      guard (contents v /= Just EmptyNode)
      uvarValues.(ix $ uvarToInt uvTarg) .= v
      uvarValues.(ix $ uvarToInt uvSrc)  .= UVarEliminated


mergeNodeIntoUVarVal :: UVar -> Node -> Seq SuspendedConstraint -> EnumerateM ()
mergeNodeIntoUVarVal uv n scs = do
  uv' <- getUVarRepresentative uv
  let idx = uvarToInt uv'
  uvarValues.(ix idx) %= intersectUVarValue (UVarUnenumerated (Just n) scs)
  newValues <- use uvarValues
  guard (contents (Sequence.index newValues idx) /= Just EmptyNode)


---------------------
-------- Variant maintainer
---------------------

-- This thing here might be a performance issue. UPDATE: Yes it is; clocked at 1/3 the time and 1/2 the
-- allocations of enumerateFully
--
-- It exists because it was easier to code / might actually be faster
-- to update referenced uvars here than inline in firstExpandableUVar.
-- There is no Sequence.foldMapWithIndexM.
refreshReferencedUVars :: EnumerateM ()
refreshReferencedUVars = do
  values <- use uvarValues
  updated <- traverse (\case UVarUnenumerated n scs ->
                               UVarUnenumerated n <$>
                                   mapM (\sc -> SuspendedConstraint (scGetPathTrie sc)
                                                                       <$> getUVarRepresentative (scGetUVar sc))
                                        scs

                             x                      -> return x)
                      values

  uvarValues .= updated


---------------------
-------- Core enumeration algorithm
---------------------

enumerateNode :: Seq SuspendedConstraint -> Node -> EnumerateM TermFragment
enumerateNode _   EmptyNode = mzero
enumerateNode scs n         =
  let (hereConstraints, descendantConstraints) = Sequence.partition (\(SuspendedConstraint pt _) -> isTerminalPathTrie pt) scs
  in case hereConstraints of
       Sequence.Empty -> case n of
                           Mu _    -> TermFragmentUVar <$> addUVarValue (Just n)
                           Node es -> do rseed <- use randomSeed
                                         case rseed of
                                           Nothing -> enumerateEdge scs =<< lift es
                                           Just seed -> do
                                             (e, seed') <- lift (shuffle seed es)
                                             randomSeed .= Just seed'
                                             enumerateEdge scs e

                           _       -> error $ "enumerateNode: unexpected node " <> show n

       (x :<| xs)     -> do reps <- mapM (getUVarRepresentative . scGetUVar) hereConstraints
                            forM_ xs $ \sc -> uvarRepresentative %= UnionFind.union (scGetUVar x) (scGetUVar sc)
                            uv <- getUVarRepresentative (scGetUVar x)
                            mapM_ (assimilateUvarVal uv) reps

                            mergeNodeIntoUVarVal uv n descendantConstraints
                            return $ TermFragmentUVar uv
  where
    shuffle :: StdGen -> [Edge] -> [(Edge, StdGen)]
    shuffle g [] = []
    shuffle g es = do
      let (i, g') = uniformR (0, length es - 1) g
      let es' = let (h, t) = splitAt i es in h ++ drop (i+1) t
      return (es !! i, g') `mplus` shuffle g' es'

enumerateEdge :: Seq SuspendedConstraint -> Edge -> EnumerateM TermFragment
enumerateEdge scs e = do
  let highestConstraintIndex = getMax $ foldMap (\sc -> Max $ fromMaybe (-1) $ getMaxNonemptyIndex $ scGetPathTrie sc) scs
  guard $ highestConstraintIndex < length (edgeChildren e)

  newScs <- Sequence.fromList <$> mapM pecToSuspendedConstraint (unsafeGetEclasses $ edgeEcs e)
  let scs' = scs <> newScs
  TermFragmentNode (edgeSymbol e) <$> imapM (\i n -> enumerateNode (descendScs i scs') n) (edgeChildren e)


---------------------
-------- Enumeration-loop control
---------------------

data ExpandableUVarResult = ExpansionStuck | ExpansionDone | ExpansionNext !UVar

-- Can speed this up with bitvectors
firstExpandableUVar :: EnumerateM ExpandableUVarResult
firstExpandableUVar = do
    values <- use uvarValues
    -- check representative uvars because only representatives are updated
    candidateMaps <- mapM (\i -> do rep <- getUVarRepresentative (intToUVar i)
                                    v <- getUVarValue rep
                                    case v of
                                        (UVarUnenumerated (Just (Mu _)) Sequence.Empty) -> return IntMap.empty
                                        (UVarUnenumerated (Just (Mu _)) _             ) -> return $ IntMap.singleton (uvarToInt rep) (Any False)
                                        (UVarUnenumerated (Just _)      _)              -> return $ IntMap.singleton (uvarToInt rep) (Any False)
                                        _                                               -> return IntMap.empty)
                              [0..(Sequence.length values - 1)]
    let candidates = IntMap.unions candidateMaps

    if IntMap.null candidates then
      return ExpansionDone
     else do
      let ruledOut = foldMap
                      (\case (UVarUnenumerated _ scs) -> foldMap
                                                             (\sc -> IntMap.singleton (uvarToInt $ scGetUVar sc) (Any True))
                                                             scs

                             _                         -> IntMap.empty)
                      values

      let unconstrainedCandidateMap = IntMap.filter (not . getAny) (ruledOut <> candidates)
      case IntMap.lookupMin unconstrainedCandidateMap of
        Nothing     -> return ExpansionStuck
        Just (i, _) -> return $ ExpansionNext $ intToUVar i



enumerateOutUVar :: UVar -> EnumerateM TermFragment
enumerateOutUVar uv = do UVarUnenumerated (Just n) scs <- getUVarValue uv
                         uv' <- getUVarRepresentative uv

                         t <- case n of
                                Mu _ -> enumerateNode scs (unfoldOuterRec n)
                                _    -> enumerateNode scs n


                         uvarValues.(ix $ uvarToInt uv') .= UVarEnumerated t
                         refreshReferencedUVars
                         return t

enumerateOutFirstExpandableUVar :: EnumerateM ()
enumerateOutFirstExpandableUVar = do
  muv <- firstExpandableUVar
  case muv of
    ExpansionNext uv -> void $ enumerateOutUVar uv
    ExpansionDone    -> mzero
    ExpansionStuck   -> mzero

enumerateFully :: EnumerateM ()
enumerateFully = do
  muv <- firstExpandableUVar
  case muv of
    ExpansionStuck   -> mzero
    ExpansionDone    -> return ()
    ExpansionNext uv -> do UVarUnenumerated (Just n) scs <- getUVarValue uv
                           if scs == Sequence.Empty then
                             case n of
                               Mu _ -> return ()
                               _    -> enumerateOutUVar uv >> enumerateFully
                            else
                             enumerateOutUVar uv >> enumerateFully

---------------------
-------- Expanding an enumerated term fragment into a term
---------------------

expandTermFrag :: TermFragment -> EnumerateM Term
expandTermFrag (TermFragmentNode s ts) = Term s <$> mapM expandTermFrag ts
expandTermFrag (TermFragmentUVar uv)   = do val <- getUVarValue uv
                                            case val of
                                              UVarEnumerated t                 -> expandTermFrag t
                                              UVarUnenumerated (Just (Mu _)) _ -> return $ Term "Mu" []
                                              _                                -> error "expandTermFrag: Non-recursive, unenumerated node encountered"

expandUVar :: UVar -> EnumerateM Term
expandUVar uv = do UVarEnumerated t <- getUVarValue uv
                   expandTermFrag t


---------------------
-------- Full enumeration
---------------------

getAllTruncatedTerms :: Node -> [Term]
getAllTruncatedTerms n = map (termFragToTruncatedTerm . fst) $
                         flip runEnumerateM (initEnumerationState Nothing n) $ do
                           enumerateFully
                           getTermFragForUVar (intToUVar 0)

getAllTerms :: Node -> [Term]
getAllTerms n = map fst $ flip runEnumerateM (initEnumerationState Nothing n) $ do
                  enumerateFully
                  expandUVar (intToUVar 0)

sampleTerm :: Int -> Node -> Term
sampleTerm seed n = head $ map fst $ flip runEnumerateM (initEnumerationState (Just seed) n) $ do
                      enumerateFully
                      expandUVar (intToUVar 0)

-- | Inefficient enumeration
--
-- For ECTAs with 'Mu' nodes may produce an infinite list or may loop indefinitely, depending on the ECTAs. For example, for
--
-- > createMu $ \r -> Node [Edge "f" [r], Edge "a" []]
--
-- it will produce
--
-- > [ Term "a" []
-- > , Term "f" [Term "a" []]
-- > , Term "f" [Term "f" [Term "a" []]]
-- > , ...
-- > ]
--
-- This happens to work currently because non-recursive edges are interned before recursive edges.
--
-- TODO: It would be much nicer if this did fair enumeration. It would avoid the beforementioned dependency on interning
-- order, and it would give better enumeration for examples such as
--
-- > Node [Edge "h" [
-- >     createMu $ \r -> Node [Edge "f" [r], Edge "a" []]
-- >   , createMu $ \r -> Node [Edge "g" [r], Edge "b" []]
-- >   ]]
--
-- This will currently produce
--
-- > [ Term "h" [Term "a" [], Term "b" []]
-- > , Term "h" [Term "a" [], Term "g" [Term "b" []]]
-- > , Term "h" [Term "a" [], Term "g" [Term "g" [Term "b" []]]]
-- > , ..
-- > ]
--
-- where it always unfolds the /second/ argument to @h@, never the first.
naiveDenotation :: Node -> [Term]
naiveDenotation = naiveDenotationBounded Nothing

-- | set a boundary on the depth of Mu node unfolding
-- if the boundary is set to @Just n@, then @n@ levels of Mu node unfolding will be performed
-- if the boundary is set to @Nothing@, then no boundary is set and the Mu nodes will be always unfolded
naiveDenotationBounded :: Maybe Int -> Node -> [Term]
naiveDenotationBounded maxDepth node = Pipes.toList $ every (go maxDepth node)
  where
    -- | Note that this code uses the decision that f(a,a) does not satisfy the constraint 0.0=1.0 because those paths are empty.
    --   It would be equally valid to say that it does.
    ecsSatisfied :: Term -> EqConstraints -> Bool
    ecsSatisfied t ecs = all (\ps -> isJust (getPath (head ps) t) && all (\p' -> getPath (head ps) t == getPath p' t) ps)
                             (map unPathEClass $ unsafeGetEclasses ecs)

    go :: Maybe Int -> Node -> ListT Identity Term
    go _       EmptyNode = mzero
    go mbDepth n@(Mu _)  = case mbDepth of
                             Nothing            -> go Nothing (unfoldOuterRec n)
                             Just d | d <= 0    -> mzero
                                    | otherwise -> go (Just $ d - 1) (unfoldOuterRec n)
    go _       (Rec _)   = error "naiveDenotation: unexpected Rec"
    go mbDepth (Node es) = do
      e <- Select $ each es

      children <- mapM (go mbDepth) (edgeChildren e)

      let res = Term (edgeSymbol e) children
      guard $ ecsSatisfied res (edgeEcs e)
      return res