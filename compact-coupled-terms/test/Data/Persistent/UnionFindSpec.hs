module Data.Persistent.UnionFindSpec ( spec ) where

import Control.Monad.State ( State, evalState, MonadState(..), modify )
import Control.Monad.Writer ( WriterT(..), MonadWriter(..) )
import Data.Equivalence.Monad ( EquivM, runEquivM, equate, equivalent )

import Test.Hspec
import Test.QuickCheck

import Data.Persistent.UnionFind


-----------------------------------------------------------


--------------------------------------------------------------
--------------------------- Commands -------------------------
--------------------------------------------------------------

type EquivTestM s = WriterT [Bool] (EquivM s [UVar] UVar)

-- Needed to work with ST type constraints
newtype ForAllEquivM c v a = ForAllEquivM { unForAllEquivM :: forall s. EquivM s c v a }

runEquivTestM :: (forall s. EquivTestM s a) -> (a, [Bool])
runEquivTestM = \m -> runEquivM (:[]) (++) (unForAllEquivM $ runWriterT' m)
  where
    runWriterT' :: (forall s. EquivTestM s a) -> ForAllEquivM [UVar] UVar (a, [Bool])
    runWriterT' m = ForAllEquivM $ runWriterT m

type PersistentUFTestM = WriterT [Bool] (State UnionFind)

runPersistentUFTestM :: PersistentUFTestM a -> (a, [Bool])
runPersistentUFTestM m = evalState (runWriterT m) empty

data UnionFindCommand = Union      UVar UVar
                      | CheckEquiv UVar UVar
  deriving ( Show )


interpCommandEquiv :: UnionFindCommand -> EquivTestM s ()
interpCommandEquiv (Union      uv1 uv2) = equate uv1 uv2
interpCommandEquiv (CheckEquiv uv1 uv2) = tell . (:[]) =<< equivalent uv1 uv2

interpCommandPersistentUF :: UnionFindCommand -> PersistentUFTestM ()
interpCommandPersistentUF (Union      uv1 uv2) = modify (union uv1 uv2)
interpCommandPersistentUF (CheckEquiv uv1 uv2) = do uf <- get
                                                    let (uv1Rep, uf')  = find uv1 uf
                                                    let (uv2Rep, uf'') = find uv2 uf'
                                                    put uf''
                                                    tell [uv1Rep == uv2Rep]



--------------------------------------------------------------
-------------------------- Generators ------------------------
--------------------------------------------------------------

instance Arbitrary UVar where
  arbitrary = intToUVar <$> chooseInt (0, 10)
  shrink _ = []

instance Arbitrary UnionFindCommand where
  arbitrary = oneof [ Union      <$> arbitrary <*> arbitrary
                    , CheckEquiv <$> arbitrary <*> arbitrary
                    ]

  shrink _ = []

--------------------------------------------------------------
----------------------------- Main ---------------------------
--------------------------------------------------------------

spec :: Spec
spec = do
  it "random stream of union/check-equiv commands gives same result as EquivM library" $
    property $ \cmds ->    runEquivTestM        (mapM_ @[] interpCommandEquiv        cmds)
                        == runPersistentUFTestM (mapM_ @[] interpCommandPersistentUF cmds)