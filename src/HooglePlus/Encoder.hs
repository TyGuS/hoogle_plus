{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HooglePlus.Encoder where

import Control.Monad.State
import Z3.Monad
import qualified Z3.Base as Z3

createType :: MonadZ3 z3 => z3 Sort
createType = do
    name <- mkStringSymbol "Type"
    aName <- mkStringSymbol "a"
    maybeName <- mkStringSymbol "maybe"
    listName <- mkStringSymbol "list"
    aCons <- mkConstructor aName aName []
    m1 <- mkStringSymbol "m1"
    mCons <- mkConstructor maybeName maybeName [(m1, Nothing, 0)] -- 0 means this is recursive type
    l1 <- mkStringSymbol "l1"
    lCons <- mkConstructor listName listName [(l1, Nothing, 0)]
    mkDatatype name [aCons, mCons, lCons]

-- P = [Array('P' + str(i), Type, IntSort()) for i in range(MAX_LEN)] # one P variable per time stamp
createPlaces :: MonadZ3 z3 => Int -> z3 [AST]
createPlaces l = do
    -- l <- loc <$> get
    int <- mkIntSort
    typ <- createType
    parr <- mkArraySort typ int
    mapM (createPlaceAt parr) [0..l]
  where
    createPlaceAt parr i = do
        pi <- mkStringSymbol ("p" ++ show i)
        mkConst pi parr

-- a -> b -> c
-- this is a micro test example
-- pass the type signatures as the parameter
createFunc :: MonadZ3 z3 => Int -> [AST] -> z3 ()
createFunc l places = do
    -- transition f :: t -> Maybe t -> t
    mapM_ createTransitionF [0..(l-1)]
    mapM_ createTransitionL [0..(l-1)]
    mapM_ createTransitionC [0..(l-1)]
  where
    createTransitionF i = do
        f <- mkStringSymbol ("tF" ++ show i)
        bool <- mkBoolSort
        fc <- mkConst f bool
        {-
        Implies (tF[i], 
            Exists([t], 
              And (P[i][t] >= 1, 
                   P[i][Type.maybe(t)] >= 1, 
                   P[i+1] == Store(P[i], Type.maybe(t), P[i][Type.maybe(t)] - 1)))) 
        -}
        typ <- createType
        maybeName <- mkStringSymbol "maybe"
        [aDecl, mDecl, lDecl] <- getDatatypeSortConstructors typ
        -- mDecl <- mkFuncDecl maybeName [typ] typ
        t <- mkStringSymbol "t"
        tc <- mkConst t typ
        uno <- mkIntNum 1

        arg0 <- mkSelect (places !! i) tc
        hasArg0 <- mkGe arg0 uno
        argApp <- mkApp mDecl [tc]
        arg1 <- mkSelect (places !! i) argApp
        hasArg1 <- mkGe arg1 uno
        pBefore <- mkSelect (places !! i) argApp
        decP <- mkSub [pBefore, uno]
        pAfter <- mkStore (places !! i) argApp decP
        changeP <- mkEq (places !! (i+1)) pAfter
        canFire <- mkAnd [hasArg0, hasArg1, changeP] >>= mkExists [] [t] [typ] >>= mkImplies fc
        optimizeAssert canFire

    createTransitionL i = do
        l <- mkStringSymbol ("tL" ++ show i)
        bool <- mkBoolSort
        lc <- mkConst l bool
        {-
        '''l :: List t -> Maybe t'''
          t  = Const('t', Type)
          return Implies (tL[i], 
                    Exists([t], 
                      And (P[i][Type.list(t)] >= 1, 
                           P[i+1] == Store(Store(P[i], 
                                            Type.list(t), P[i][Type.list(t)] - 1),
                                            Type.maybe(t), P[i][Type.maybe(t)] + 1))))
        -}
        typ <- createType
        maybeName <- mkStringSymbol "maybe"
        listName <- mkStringSymbol "list"
        [aDecl, mDecl, lDecl] <- getDatatypeSortConstructors typ
        -- mDecl <- mkFuncDecl maybeName [typ] typ
        -- lDecl <- mkFuncDecl listName [typ] typ
        t <- mkStringSymbol "t"
        tc <- mkConst t typ
        uno <- mkIntNum 1

        lApp <- mkApp lDecl [tc]
        mApp <- mkApp mDecl [tc]
        ltoken <- mkSelect (places !! i) lApp
        mtoken <- mkSelect (places !! i) mApp
        hasArg0 <- mkGe ltoken uno
        decLt <- mkSub [ltoken, uno]
        incMt <- mkAdd [mtoken, uno]
        pDecLt <- mkStore (places !! i) lApp decLt
        pIncMt <- mkStore pDecLt mApp incMt
        changeP <- mkEq (places !! (i+1)) pIncMt
        canFire <- mkAnd [hasArg0, changeP] >>= mkExists [] [t] [typ] >>= mkImplies lc 
        optimizeAssert canFire

    createTransitionC i = do
        f <- mkStringSymbol ("tC" ++ show i)
        bool <- mkBoolSort
        fc <- mkConst f bool
        {-
        '''c :: List (Maybe t) -> List t'''
          t  = Const('t', Type)
          return Implies (tC[i], 
                    Exists([t], 
                      And (P[i][Type.list(Type.maybe(t))] >= 1, 
                           P[i+1] == Store(Store(P[i], 
                                            Type.list(Type.maybe(t)), P[i][Type.list(Type.maybe(t))] - 1),
                                            Type.list(t), P[i][Type.list(t)] + 1))))
        -}
        typ <- createType
        maybeName <- mkStringSymbol "maybe"
        listName <- mkStringSymbol "list"
        [aDecl, mDecl, lDecl] <- getDatatypeSortConstructors typ
        -- mDecl <- mkFuncDecl maybeName [typ] typ
        -- lDecl <- mkFuncDecl listName [typ] typ
        t <- mkStringSymbol "t"
        tc <- mkConst t typ
        uno <- mkIntNum 1

        lApp <- mkApp lDecl [tc]
        mApp <- mkApp mDecl [tc]
        arg0 <- mkApp lDecl [mApp]
        arg0token <- mkSelect (places !! i) arg0
        hasArg0 <- mkGe arg0token uno
        retToken <- mkSelect (places !! i) lApp
        decLmt <- mkSub [arg0token, uno]
        incLt <- mkAdd [retToken, uno]
        pDecLt <- mkStore (places !! i) arg0 decLmt
        pIncMt <- mkStore pDecLt lApp incLt
        changeP <- mkEq (places !! (i+1)) pIncMt
        canFire <- mkAnd [hasArg0, changeP] >>= mkExists [] [t] [typ] >>= mkImplies fc
        optimizeAssert canFire

test :: MonadZ3 z3 => Int -> z3 ()
test i = do
    optimizePush
    liftIO $ print $ "trying length of " ++ show i
    -- create constraints
    ps <- createPlaces i
    createFunc i ps
    -- set initial state
    -- User query: a -> List (Maybe a) -> a
    typ <- createType
    aName <- mkStringSymbol "a"
    lName <- mkStringSymbol "list"
    mName <- mkStringSymbol "maybe"
    [aDecl, mDecl, lDecl] <- getDatatypeSortConstructors typ
    -- aDecl <- mkFuncDecl aName [] typ
    -- lDecl <- mkFuncDecl lName [typ] typ
    -- mDecl <- mkFuncDecl mName [typ] typ
    aApp <- mkApp aDecl []
    mApp <- mkApp mDecl [aApp]
    lApp <- mkApp lDecl [mApp]
    int <- mkIntSort
    parr <- mkArraySort typ int
    zero <- mkIntNum 0
    uno <- mkIntNum 1
    emptyArr <- mkConstArray typ zero
    addArg0 <- mkStore emptyArr aApp uno
    addArg1 <- mkStore addArg0 lApp uno
    initial <- mkEq (ps !! 0) addArg1
    optimizeAssert initial
    -- set final state
    addRet <- mkStore emptyArr aApp uno
    final <- mkEq (ps !! i) addRet
    optimizeAssert final
    -- at least one transition is fired at each time
    {-
      s.add (Or(tF[i], tL[i], tC[i]))
      s.add (Not(And(tF[i], tL[i])))
      s.add (Not(And(tF[i], tC[i])))
      s.add (Not(And(tL[i], tC[i]))) 
    -}
    mapM_ atLeastOne [0..(i-1)]

    assts <- optimizeGetAssertions
    asstStr <- mapM astToString assts
    liftIO $ mapM_ putStrLn asstStr
    -- check by the solver
    res <- optimizeCheck
    case res of 
        Sat -> do
            model <- optimizeGetModel
            modelStr <- modelToString model
            liftIO $ putStrLn modelStr
        _ -> when (i < 10) (optimizePop 0 >> test (i+1))
  where
    atLeastOne t = do
        bool <- mkBoolSort
        f <- mkStringSymbol ("tF" ++ show t)
        fc <- mkConst f bool
        c <- mkStringSymbol ("tC" ++ show t)
        cc <- mkConst c bool
        l <- mkStringSymbol ("tL" ++ show t)
        lc <- mkConst l bool
        mkOr [fc, cc, lc] >>= optimizeAssert
        mkAnd [fc, cc] >>= mkNot >>= optimizeAssert
        mkAnd [lc, cc] >>= mkNot >>= optimizeAssert
        mkAnd [fc, lc] >>= mkNot >>= optimizeAssert

runTest :: IO ()
runTest = evalZ3 (test 1)