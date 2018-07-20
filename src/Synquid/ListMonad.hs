{-# LANGUAGE ScopedTypeVariables,FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Synquid.ListMonad where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Cont
import Control.Applicative
 
-- The monadic list type
data MList' m a = MNil | a `MCons` MList m a
type MList m a  = m (MList' m a)
 
-- This can be directly used as a monad transformer
newtype ListT m a = ListT { runListT :: MList m a }
 
-- A "lazy" run function, which only calculates the first solution.
runListT' :: Functor m => ListT m a -> m (Maybe (a, ListT m a))
runListT' (ListT m) = fmap g m where
  g MNil = Nothing
  g (x `MCons` xs) = Just (x, ListT xs)
 
-- In ListT from Control.Monad this one is the data constructor ListT, so sadly, this code can't be a drop-in replacement.
liftList :: Monad m => [a] -> ListT m a
liftList [] = ListT $ return MNil
liftList (x:xs) = ListT . return $ x `MCons` (runListT $ liftList xs)
 
instance Functor m => Functor (ListT m) where
  fmap f (ListT m) = ListT $ fmap (fmap f) m
 
instance Functor m => Functor (MList' m) where  
  fmap _ MNil = MNil
  fmap f (x `MCons` xs) = f x `MCons` fmap (fmap f) xs
 
instance (Applicative m, Monad m) => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance (Alternative m, Monad m) => Alternative (ListT m) where
    (<|>) = mplus
    empty = mzero

-- Why on earth isn't Monad declared `class Functor m => Monad m'?
-- I assume that a monad is always a functor, so the contexts 
-- get a little larger than actually necessary
instance (Functor m, Monad m) => Monad (ListT m) where
  return x = ListT . return $ x `MCons` return MNil
  m >>= f = joinListT $ fmap f m
 
instance MonadTrans ListT where
  lift = ListT . liftM (`MCons` return MNil)
 
instance (Functor m, Monad m, Alternative m) => MonadPlus (ListT m) where
  mzero = liftList []
  (ListT xs) `mplus` (ListT ys) = ListT $ xs `mAppend` ys
 
-- Implemenation of join
joinListT :: (Functor m, Monad m) => ListT m (ListT m a) -> ListT m a
joinListT (ListT xss) = ListT . joinMList $ fmap (fmap runListT) xss
 
joinMList :: (Functor m, Monad m) => MList m (MList m a) -> MList m a
joinMList = (=<<) joinMList'
 
joinMList' :: (Functor m, Monad m) => MList' m (MList m a) -> MList m a
joinMList' MNil = return MNil
joinMList' (x `MCons` xs) = x `mAppend` joinMList xs
 
mAppend :: (Functor m, Monad m) => MList m a -> MList m a -> MList m a
mAppend xs ys = (`mAppend'` ys) =<< xs
 
mAppend' :: (Functor m, Monad m) => MList' m a -> MList m a -> MList m a
mAppend' MNil           ys = ys
mAppend' (x `MCons` xs) ys = return $ x `MCons` mAppend xs ys

-- These things typecheck, but I haven't made sure what they do is sensible.
-- (callCC almost certainly has to be changed in the same way as throwError)
instance (MonadIO m, Functor m) => MonadIO (ListT m) where
  liftIO = lift . liftIO
 
instance (MonadReader s m, Functor m) => MonadReader s (ListT m) where
  ask     = lift ask
  local f = ListT . local f . runListT
 
instance (MonadState s m, Functor m) => MonadState s (ListT m) where
  get = lift get
  put = lift . put
 
instance (MonadCont m, Functor m) => MonadCont (ListT m) where
  callCC f = ListT $
    callCC $ \c ->
      runListT . f $ \a -> 
        ListT . c $ a `MCons` return MNil
 
instance (MonadError e m, Functor m) => MonadError e (ListT m) where
  throwError       = lift . throwError
{- This (perhaps more straightforward) implementation has the disadvantage
   that it only catches errors that occur at the first position of the 
   list.
  m `catchError` h = ListT $ runListT m `catchError` \e -> runListT (h e)
-}
  -- m `catchError` h = ListT $ runListT m `catchError` \e -> runListT (h e)
  -- This is better because errors are caught everywhere in the list.
  (m :: ListT m a) `catchError` h = ListT . deepCatch . runListT $ m 
      where
    deepCatch :: MList m a -> MList m a
    deepCatch ml = fmap deepCatch' ml `catchError` \e -> runListT (h e)
 
    deepCatch' :: MList' m a -> MList' m a
    deepCatch' MNil = MNil 
    deepCatch' (x `MCons` xs) = x `MCons` deepCatch xs