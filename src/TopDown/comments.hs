module comments where

  -- collect all the component types (which we might use to fill the holes)
  -- let components = Map.toList (env ^. symbols)

  -- if (hasHold p)
  -- find all functions that unify with goal type
  -- unifiedFuncs <- getUnifiedFunctions' env messageChan components goalType :: CompsSolver IO [(Id, SType)]

  -- Int -> Int
  -- unifiedFuncs should first return arg0
  -- checks if that's ground and returns that
  -- if it gets to something else, like `add`
    -- then 

{-
  (x:xs)
    x is new program 
    do stuff with x (decide if complete or not)
      and check that it matches the examples
        return if that's the case
      if not ground, do the newFunc that we wrote
          recurse with (xs ++ newStuff)
-}

  -- unifiedFunc@(id, schema) <- getUnifiedFunctions' env messageChan components goalType mzero :: TopDownBackTrack IO (Id, SType)
  -- unifiedFuncs <- lift $ getUnifiedFunctions' env messageChan components goalType :: TopDownBackTrack IO [(Id, SType)]
  
  -- unifiedFuncs <- lift $ lift $ getUnifiedFunctions''' env messageChan components goalType :: TopDownBackTrack IO [RProgram]
  
  -- program <- choices unifiedFuncs :: TopDownBackTrack IO RProgram

  -- when (hasHole program) $ do
  --   -- fillHoles :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]
  --   newHoledPrograms <- lift $ lift $ fillHoles env messageChan program :: TopDownBackTrack IO [RProgram]
  --   st <- get
  --   st `mplus` (choices newHoledPrograms)

  -- guard (not $ hasHole program)
  -- return program










      -- fillHoles :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]
    -- fillHoles env messageChan (Program (PApp f args) typ) = return filledPrograms
    --   where
    --     argUnifications = map (fillHoles env messageChan) args :: [[RProgram]]
    --     cartesianArgs = sequence argUnifications :: [[RProgram]]
    --     filledPrograms = [Program { content = PApp f filledArgs, typeOf = typ } | filledArgs <- cartesianArgs]

    -- fillHoles env messageChan (Program PHole typ)         = getUnifiedFunctions''' env messageChan (env ^. compoenents) (shape typ)
    -- fillHoles _   _           program                     = return [program]














{-
      f [ ((g ??::PHole ) :: is a PApp) ((g ??) is a PHole) ]
      f (g ??) ?? <- no need to recurse, fill the rightmost ??
      f (g ??) (h ??) <- need to recurse, call fillFirstHole on (g ??), and then (h ??) only if that failed to fill a hole
      f (g (i (k ??)) (h ??))
      cartesian product no longer needed
-}

{-
      args - we check if any of the args themselves are holes - replace first one if so
      if none are holes themselves, then go through each one at a time and see if it 
          returns Nothing or Just (if Just, you're done, if nothing, go to next one)
      
      different idea:
      try to use the fairness thing of LogicT to return first result
      https://stackoverflow.com/questions/22333634/how-do-i-convert-a-list-monadic-function-to-a-breadth-first-search
-}

{-
          fillFirstHole depth Program 
          fillFirstHole _     PHole = Just getUnified ...
          fillFirstHole d     Program (papp (id args)) = 
          fillFirstHole 1     (papp) = Nothing
          
-}

















-- length stuff: 
-- Non-ground program found: GHC.List.length (?? :: [tau56]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length ((GHC.List.!!) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: Int) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Bool.bool (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Bool) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Either.fromLeft (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Either ((({[{tau56|False}]|False} -> Int))) (tau936)) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Either.fromRight (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Either (tau939) ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Function.const (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: tau950) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Function.id (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Maybe.fromJust (?? :: Maybe ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Maybe.fromMaybe (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Maybe ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.head (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.last (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.product (?? :: @@hplusTC@@Num ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms


          (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))


-- /tmp/12189c77-1501-4b56-8ecf-7eb285c2fa15.hs:14:117: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))
--    |
-- 14 | ghcCheckedFunction = \arg0 -> GHC.List.sum (GHC.List.sum (GHC.List.last []) []) (GHC.List.take (GHC.List.sum arg0) (Data.Function.const arg0 ))
--    |                                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^
-- <interactive>:1:97: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))


-- /tmp/c43e00c2-952b-4e00-9f4a-10865e9c6a2c.hs:14:117: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))
--    |
-- 14 | ghcCheckedFunction = \arg0 -> GHC.List.sum (GHC.List.sum (GHC.List.last []) []) (GHC.List.take (GHC.List.sum arg0) (Data.Function.const arg0 ))
--    |                                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^
-- <interactive>:1:97: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))


-- /tmp/def66847-4999-49d9-a18d-11a9860dbe31.hs:14:117: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))
--    |
-- 14 | ghcCheckedFunction = \arg0 -> GHC.List.sum (GHC.List.sum (GHC.List.last []) []) (GHC.List.take (GHC.List.sum arg0) (Data.Function.const arg0 ))
--    |                                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^
-- <interactive>:1:97: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))


-- /tmp/e112e782-15be-4d06-aabf-e40fcd04dd0d.hs:14:117: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))
--    |
-- 14 | ghcCheckedFunction = \arg0 -> GHC.List.sum (GHC.List.sum (GHC.List.last []) []) (GHC.List.take (GHC.List.sum arg0) (Data.Function.const arg0 ))
--    |                                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^
-- <interactive>:1:97: error:
--     • Couldn't match expected type ‘[a1]’
--                   with actual type ‘b0 -> [Int]’
--     • Probable cause: ‘const’ is applied to too few arguments
--       In the second argument of ‘take’, namely ‘(const arg0)’
--       In the second argument of ‘GHC.List.sum’, namely
--         ‘(take (GHC.List.sum arg0) (const arg0))’
--       In the expression:
--         GHC.List.sum
--           (GHC.List.sum (last []) []) (take (GHC.List.sum arg0) (const arg0))






-- Non-ground program found: (?? :: Int). adding 15 newHoledPrograms
-- Non-ground program found: (?? :: [Int]) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (?? :: Int) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (?? :: Int) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (?? :: [tau56]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (?? :: Int). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Int) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft ((GHC.List.!!) (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Bool.bool (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: Bool) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Either.fromLeft (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: Either ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int)))) (tau272)) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Either.fromRight (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: Either (tau275) ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Function.const (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: tau286) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Function.id (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))) (?? :: Maybe ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.head (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.last (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({Int|False}) ({tau23|False})|False} -> Int))]) (?? :: Int) (?? :: Either (Int) (tau23))) (?? :: Either (Int) (tau23)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight ((GHC.List.!!) (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Bool.bool (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: Bool) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Either.fromLeft (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: Either ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int)))) (tau355)) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Either.fromRight (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: Either (tau358) ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Function.const (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: tau369) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Function.id (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))) (?? :: Maybe ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.head (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.last (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Either ({tau26|False}) ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Either (tau26) (Int))) (?? :: Either (tau26) (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const ((GHC.List.!!) (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Bool.bool (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: Bool) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Either.fromLeft (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: Either ((({Int|False} -> ({tau37|False} -> Int)))) (tau438)) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Either.fromRight (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: Either (tau441) ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Function.const (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: tau452) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Function.id (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({tau37|False} -> Int))) (?? :: Maybe ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.head (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.last (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.const (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({tau37|False} -> Int))))) (?? :: [({Int|False} -> ({tau37|False} -> Int))]) (?? :: Int) (?? :: tau37)) (?? :: tau37). adding 14 newHoledPrograms

-- Non-ground program found: Data.Function.id ((GHC.List.!!) (?? :: [({Int|False} -> Int)]) (?? :: Int) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Bool.bool (?? :: ({Int|False} -> Int)) (?? :: ({Int|False} -> Int)) (?? :: Bool) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Either.fromLeft (?? :: ({Int|False} -> Int)) (?? :: Either ((({Int|False} -> Int))) (tau521)) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Either.fromRight (?? :: ({Int|False} -> Int)) (?? :: Either (tau524) ((({Int|False} -> Int)))) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Function.const (?? :: ({Int|False} -> Int)) (?? :: tau535) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Function.id (?? :: ({Int|False} -> Int)) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> Int)))) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (Data.Maybe.fromMaybe (?? :: ({Int|False} -> Int)) (?? :: Maybe ((({Int|False} -> Int)))) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.head (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.last (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> Int)))) (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> Int)))) (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> Int)))) (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Function.id (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> Int)))) (?? :: [({Int|False} -> Int)]) (?? :: Int)). adding 14 newHoledPrograms

-- Non-ground program found: Data.Maybe.fromJust ((GHC.List.!!) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Int) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Bool.bool (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: Bool) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Either.fromLeft (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: Either ((({Maybe ({Int|False})|False} -> Int))) (tau604)) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Either.fromRight (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: Either (tau607) ((({Maybe ({Int|False})|False} -> Int)))) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Function.const (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: tau618) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Function.id (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Maybe.fromJust (?? :: Maybe ((({Maybe ({Int|False})|False} -> Int)))) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (Data.Maybe.fromMaybe (?? :: ({Maybe ({Int|False})|False} -> Int)) (?? :: Maybe ((({Maybe ({Int|False})|False} -> Int)))) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.head (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.last (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Maybe ({Int|False})|False} -> Int)))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Maybe ({Int|False})|False} -> Int)))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.product (?? :: @@hplusTC@@Num ((({Maybe ({Int|False})|False} -> Int)))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (GHC.List.sum (?? :: @@hplusTC@@Num ((({Maybe ({Int|False})|False} -> Int)))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))). adding 14 newHoledPrograms

-- Non-ground program found: Data.Maybe.fromMaybe ((GHC.List.!!) (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Bool.bool (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: Bool) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Either.fromLeft (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: Either ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int)))) (tau687)) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Either.fromRight (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: Either (tau690) ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Function.const (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: tau701) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Function.id (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Maybe ({Int|False})|False} -> Int))) (?? :: Maybe ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.head (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.last (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Maybe ({Int|False})|False} -> Int))))) (?? :: [({Int|False} -> ({Maybe ({Int|False})|False} -> Int))]) (?? :: Int) (?? :: Maybe (Int))) (?? :: Maybe (Int)). adding 14 newHoledPrograms

-- Non-ground program found: GHC.List.head ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: Int) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> Int)) (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Bool) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Either ((({[{Int|False}]|False} -> Int))) (tau770)) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Either (tau773) ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Function.const (?? :: ({[{Int|False}]|False} -> Int)) (?? :: tau784) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Function.id (?? :: ({[{Int|False}]|False} -> Int)) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Maybe ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.head (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.last (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.head (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms

-- Non-ground program found: GHC.List.last ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: Int) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> Int)) (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Bool) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Either ((({[{Int|False}]|False} -> Int))) (tau853)) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Either (tau856) ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Function.const (?? :: ({[{Int|False}]|False} -> Int)) (?? :: tau867) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Function.id (?? :: ({[{Int|False}]|False} -> Int)) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> Int)) (?? :: Maybe ((({[{Int|False}]|False} -> Int)))) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.head (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.last (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.last (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> Int)))) (?? :: [({[{Int|False}]|False} -> Int)]) (?? :: [Int])). adding 14 newHoledPrograms

-- Non-ground program found: GHC.List.length ((GHC.List.!!) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: Int) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Bool.bool (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Bool) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Either.fromLeft (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Either ((({[{tau56|False}]|False} -> Int))) (tau936)) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Either.fromRight (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Either (tau939) ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Function.const (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: tau950) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Function.id (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Maybe.fromJust (?? :: Maybe ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (Data.Maybe.fromMaybe (?? :: ({[{tau56|False}]|False} -> Int)) (?? :: Maybe ((({[{tau56|False}]|False} -> Int)))) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.head (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.last (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.product (?? :: @@hplusTC@@Num ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.length (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{tau56|False}]|False} -> Int)))) (?? :: [({[{tau56|False}]|False} -> Int)]) (?? :: [tau56])). adding 14 newHoledPrograms

-- Non-ground program found: GHC.List.maximum ((GHC.List.!!) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: Int) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Bool.bool (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Bool) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Either.fromLeft (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int)))) (tau1019)) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Either.fromRight (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either (tau1022) ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Function.const (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: tau1033) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Function.id (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Maybe.fromJust (?? :: Maybe ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (Data.Maybe.fromMaybe (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Maybe ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.head (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.last (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.product (?? :: @@hplusTC@@Num ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (GHC.List.sum (?? :: @@hplusTC@@Num ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum ((GHC.List.!!) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: Int) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Bool.bool (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Bool) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Either.fromLeft (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int)))) (tau1102)) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Either.fromRight (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either (tau1105) ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Function.const (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: tau1116) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Function.id (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Maybe.fromJust (?? :: Maybe ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (Data.Maybe.fromMaybe (?? :: ({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Maybe ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.head (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.last (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.product (?? :: @@hplusTC@@Num ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (GHC.List.sum (?? :: @@hplusTC@@Num ((({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Ord ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product ((GHC.List.!!) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: Int) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Bool.bool (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Bool) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Either.fromLeft (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int)))) (tau1185)) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Either.fromRight (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either (tau1188) ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Function.const (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: tau1199) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Function.id (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Maybe.fromJust (?? :: Maybe ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (Data.Maybe.fromMaybe (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Maybe ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.head (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.last (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.product (?? :: @@hplusTC@@Num ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.product (GHC.List.sum (?? :: @@hplusTC@@Num ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum ((GHC.List.!!) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: Int) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Bool.bool (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Bool) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Either.fromLeft (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int)))) (tau1268)) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Either.fromRight (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Either (tau1271) ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Function.const (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: tau1282) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Function.id (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Maybe.fromJust (?? :: Maybe ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (Data.Maybe.fromMaybe (?? :: ({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))) (?? :: Maybe ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.head (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.last (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.product (?? :: @@hplusTC@@Num ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: GHC.List.sum (GHC.List.sum (?? :: @@hplusTC@@Num ((({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))))) (?? :: [({@@hplusTC@@Num ({Int|False})|False} -> ({[{Int|False}]|False} -> Int))]) (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])) (?? :: [Int]). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1351)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1354) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1365) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1434)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1437) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1448) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1517)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1520) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1531) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau106)) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1600)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1603) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1614) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau109) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1683)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1686) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1697) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau120) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1766)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1769) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1780) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1849)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1852) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1863) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau1932)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau1935) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau1946) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2015)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2018) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2029) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2098)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2101) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2112) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2181)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2184) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2195) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2264)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2267) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2278) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2347)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2350) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2361) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! ((GHC.List.!!) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: Int) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Bool.bool (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Bool) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromLeft (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either ((({[{Int|False}]|False} -> ({Int|False} -> Int)))) (tau2430)) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Either.fromRight (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Either (tau2433) ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.const (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: tau2444) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Function.id (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: ({[{Int|False}]|False} -> ({Int|False} -> Int))) (?? :: Maybe ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.head (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.last (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num ((({[{Int|False}]|False} -> ({Int|False} -> Int))))) (?? :: [({[{Int|False}]|False} -> ({Int|False} -> Int))]) (?? :: [Int]) (?? :: Int)). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2513)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2516) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2527) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2596)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2599) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2610) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2679)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2682) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2693) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau189)) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2762)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2765) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2776) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau192) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2845)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2848) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2859) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau203) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau2928)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau2931) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau2942) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau3011)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau3014) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau3025) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.head (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.last (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.maximum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.minimum (?? :: @@hplusTC@@Ord ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.product (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (GHC.List.sum (?? :: @@hplusTC@@Num ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) ((GHC.List.!!) (?? :: [({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))]) (?? :: Int) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Bool.bool (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Bool) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromLeft (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int))))) (tau3094)) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Either.fromRight (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Either (tau3097) ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.const (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: tau3108) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- ^CNon-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Function.id (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (Data.Maybe.fromMaybe (?? :: ({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))) (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (Data.Maybe.fromJust (?? :: Maybe ((({Int|False} -> ({Int|False} -> ({Bool|False} -> Int)))))) (?? :: Int) (?? :: Int) (?? :: Bool)) (?? :: Bool). adding 14 newHoledPrograms











-- Non-ground program found: GHC.List.minimum (GHC.List.product (?? :: @@hplusTC@@Num ((({@@hplusTC@@Ord (((Maybe (Int) -> {Int|False})))|False} -> ({[(Maybe (Int) -> {Int|False})]|False} -> ({Maybe ({Int|False})|False} -> Int)))))) (?? :: [({@@hplusTC@@Ord (((Maybe (Int) -> {Int|False})))|False} -> ({[(Maybe (Int) -> {Int|False})]|False} -> ({Maybe ({Int|False})|False} -> Int)))]) (?? :: @@hplusTC@@Ord ((({Maybe ({Int|False})|False} -> Int)))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int))) (?? :: [({Maybe ({Int|False})|False} -> Int)]) (?? :: Maybe (Int)). adding 14 newHoledPrograms


          -- f (g ??) ?? <- no need to recurse, fill the rightmost ??
          -- f (g ??) (h ??) <- need to recurse, call fillFirstHole on (g ??), and then (h ??) only if that failed to fill a hole
          -- cartesian product no longer needed

        -- check if args contains any holes
        -- if so, fill the first one only
        -- 
        -- argUnifications <- mapM (fillFirstHole env messageChan) args :: StateT CheckerState IO [[RProgram]]
        -- let cartesianArgs = sequence argUnifications :: [[RProgram]]
        -- let filledPrograms = [Program { content = PApp f filledArgs, typeOf = typ } | filledArgs <- cartesianArgs]
        -- return filledPrograms
    -- fillFirstHole env messageChan (Program PHole typ)         = getUnifiedFunctions''' env messageChan (Map.toList (env ^. symbols)) (shape typ)


    -- determines if the result has all the appropriate arguments given the number of args















--  ?? !! ??
--  bool
--  fromLeft
--  ....
--  minimum ??
--  product ??
--  sum ??
--  (?? !! ??) !! (?? !! ??)
--  (?? ++ ??) !! (?? !! ??)
--  (?? : ??) !! (?? !! ??)
--  (bool ??) !! (?? !! ??)
--  (fromLeft ??) !! (?? !! ??)
--  ...
--  length (arg0)

--                   Int
--     (?? !! ??)  (?? ++ ??)  (bool ??) (fromLeft ??) (length ??)
--     (a !! a) (a ++ a) (bool a) (fromLeft a) (length a)
--     (b !! a)

-- Non-ground program found: (?? :: Int). adding 15 newHoledPrograms
-- Non-ground program found: (?? :: [Int]) !! (?? :: Int). adding 450 newHoledPrograms
-- Non-ground program found: Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool). adding 7425 newHoledPrograms
-- Non-ground program found: Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau23)). adding 240 newHoledPrograms
-- Non-ground program found: Data.Either.fromRight (?? :: Int) (?? :: Either (tau26) (Int)). adding 240 newHoledPrograms
-- Non-ground program found: Data.Function.const (?? :: Int) (?? :: tau37). adding 1380 newHoledPrograms
-- Non-ground program found: Data.Function.id (?? :: Int). adding 15 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromJust (?? :: Maybe (Int)). adding 18 newHoledPrograms
-- Non-ground program found: Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int)). adding 270 newHoledPrograms
-- Non-ground program found: GHC.List.head (?? :: [Int]). adding 30 newHoledPrograms
-- Non-ground program found: GHC.List.last (?? :: [Int]). adding 30 newHoledPrograms
-- Non-ground program found: GHC.List.length (?? :: [tau56]). adding 38 newHoledPrograms
-- Non-ground program found: GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]). adding 480 newHoledPrograms
-- Non-ground program found: GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int]). adding 480 newHoledPrograms
-- Non-ground program found: GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]). adding 450 newHoledPrograms
-- Non-ground program found: GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int]). adding 450 newHoledPrograms

-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! ((?? :: [Int]) !! (?? :: Int)). adding 202500 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool)). adding 3341250 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau189))). adding 108000 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Either.fromRight (?? :: Int) (?? :: Either (tau192) (Int))). adding 108000 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Function.const (?? :: Int) (?? :: tau203)). adding 621000 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Function.id (?? :: Int)). adding 6750 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Maybe.fromJust (?? :: Maybe (Int))). adding 8100 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int))). adding 121500 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.head (?? :: [Int])). adding 13500 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.last (?? :: [Int])). adding 13500 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.length (?? :: [tau222])). adding 17100 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 202500 newHoledPrograms
-- Non-ground program found: ((?? :: [[Int]]) !! (?? :: Int)) !! (GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 202500 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! ((?? :: [Int]) !! (?? :: Int)). adding 405000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool)). adding 6682500 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau189))). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Either.fromRight (?? :: Int) (?? :: Either (tau192) (Int))). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Function.const (?? :: Int) (?? :: tau203)). adding 1242000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Function.id (?? :: Int)). adding 13500 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Maybe.fromJust (?? :: Maybe (Int))). adding 16200 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int))). adding 243000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.head (?? :: [Int])). adding 27000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.last (?? :: [Int])). adding 27000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.length (?? :: [tau222])). adding 34200 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 432000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 432000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 405000 newHoledPrograms
-- Non-ground program found: ((?? :: [Int]) ++ (?? :: [Int])) !! (GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 405000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! ((?? :: [Int]) !! (?? :: Int)). adding 202500 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Bool.bool (?? :: Int) (?? :: Int) (?? :: Bool)). adding 3341250 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Either.fromLeft (?? :: Int) (?? :: Either (Int) (tau189))). adding 108000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Either.fromRight (?? :: Int) (?? :: Either (tau192) (Int))). adding 108000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Function.const (?? :: Int) (?? :: tau203)). adding 621000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Function.id (?? :: Int)). adding 6750 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Maybe.fromJust (?? :: Maybe (Int))). adding 8100 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (Data.Maybe.fromMaybe (?? :: Int) (?? :: Maybe (Int))). adding 121500 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.head (?? :: [Int])). adding 13500 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.last (?? :: [Int])). adding 13500 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.length (?? :: [tau222])). adding 17100 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.maximum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.minimum (?? :: @@hplusTC@@Ord (Int)) (?? :: [Int])). adding 216000 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.product (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 202500 newHoledPrograms
-- Non-ground program found: ((?? :: Int) : (?? :: [Int])) !! (GHC.List.sum (?? :: @@hplusTC@@Num (Int)) (?? :: [Int])). adding 202500 newHoledPrograms









  -- return Program { content = PHole, typeOf = head args }
  {-

      * list <- getUnifiedFunctions :: [RProgram`s]  (they will have holes in them)
      * add this list to a LogicT list 
      * go through this list
          * if program is ground
                return it
          * if not ground
                find holes, call getUnifiedFunctions on those holes, and reconstruct the program with those in place of the holes
                add these programs back into the list of LogicT
      
  -}



-- our new isNotGround
-- hasHole (Program p _) = case p of
--   PApp fun arg -> or (map hasHole arg)
--   PHole -> True
--   _ -> False


{-
  PApp fun args = 
  for every argument
    [unifiedFunctions]
    [unifiedFunctions]
    [unifiedFunctions]

  [[cartesian product of all of them]]
  [PAgg fun cartesian[0], PAgg fun cartesian[1], ...]

-}


-- fillHoles (Program p schema) env messageChan = case p of
  -- PApp f args -> 
  -- PHole -> getUnifiedFunctions'' env messageChan 

  -- -- Program { content = PHole, typeOf = arg } | arg <- args]
  -- program -> [program] -- [arg0]
  


-- TODO fill holes function idea
-- blah (Program p _) = case p of
--   PApp f args -> PApp f (map blah args)
--   PHole -> getUnifiedFunctions


  
  -- unless (isGround schema) $ do
  --   -- call dfs here and mplus onto the list ???
  --   let args = allArgTypes schema :: [SType]

  --   solutionsPerArg <- mapM (dfs env messageChan (depth - 1)) args :: TopDownBackTrack IO [RProgram]

  --   let putOntoList x = get `mplus` x
  --   map putOntoList solutionsPerArg
  --   return ()

  
  
--     turnFunctionIntoSolutions :: (Id, SType) -> CompsSolver IO [RProgram]
--     turnFunctionIntoSolutions (id, schema)
--       | isGround schema = return [Program { content = PSymbol id, typeOf = refineTop env schema }]
--       | depth == 0 = return []  -- stop if depth is 0
--       | otherwise = do

--         -- collect all the argument types (the holes ?? we need to fill)
--         let args = allArgTypes schema :: [SType]

--         -- recursively call DFS on the arguments' types to get the functions that return those types
--         let recurse = dfs env messageChan (depth-1) :: SType -> CompsSolver IO [RProgram]
--         solutionsPerArg <- mapM recurse args :: CompsSolver IO [[RProgram]] -- [[a,b,c], [d,e,f]]

--         -- get cartesian product of all the arguments
--         let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f]]

--         -- fill in arguments of func as RPrograms - [func a d, func a e, func a f, func b d, func b e, func b f]
--         let formatFn :: [RProgram] -> RProgram
--             formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
        
--         let finalResultList = map formatFn programsPerArg

--         return finalResultList




  -- if isGround schema
  --   then do
      
  --     -- guard (isInfixOf "length" id)
  --     -- liftIO $ print $ unifiedFunc
  --     -- st <- lift $ get :: TopDownBackTrack IO IncompletePrograms
  --     -- liftIO $ putStrLn $ show st
  --     return Program { content = PSymbol id, typeOf = refineTop env schema }
  --     -- liftIO $ print $ "here"
  --     -- liftIO $ putStrLn "afterwards!!! "
  --   else do -- not ground we should recurse
  --     lift $ modify (\st -> (id, schema):st)
  --     guard False
  --     -- logic `interleave` logic
  --     return undefined

-- --  ++::[a]->[a]->[a] ⇒ mplus::ma->ma->ma

      -- http://hackage.haskell.org/package/control-monad-omega-0.3.2/docs/Control-Monad-Omega.html
      -- [length, id, arg0, (length arg0), (length id)]
 


    -- gets first one that's ground




























  --   -- given a function that returns the type we want (goalType)
  --   -- find all the possible ways to call that function and return them as RProgram
  --   turnFunctionIntoSolutions :: (Id, SType) -> TopDownBackTrack IO RProgram
  --   turnFunctionIntoSolutions (id, schema)
  --     | isGround schema = return Program { content = PSymbol id, typeOf = refineTop env schema }
  --     | depth == 0 = return []  -- stop if depth is 0
  --     | otherwise = do

  --       -- collect all the argument types (the holes ?? we need to fill)
  --       let args = allArgTypes schema :: [SType]

  --       -- recursively call DFS on the arguments' types to get the functions that return those types
  --       let recurse = dfs env messageChan (depth-1) :: SType -> TopDownBackTrack IO RProgram
  --       solutionsPerArg <- mapM recurse args :: TopDownBackTrack IO [RProgram] -- [[a,b,c], [d,e,f]]

  --       -- get cartesian product of all the arguments
  --       let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f]]

  --       -- fill in arguments of func as RPrograms - [func a d, func a e, func a f, func b d, func b e, func b f]
  --       let formatFn :: [RProgram] -> RProgram
  --           formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
        
  --       let finalResultList = map formatFn programsPerArg

  --       return finalResultList




{--
    takes in single type (return type)          e.g. Int
    finds all functions that unify with it      e.g. [f: Int, g: Int -> Int, h: Int -> Bool -> Int]
      for each of these functions F:
        if not ground:                          e.g. [Int -> Int, Int -> Bool -> Int]
          get arguments of F                    e.g. [[Int], [Int, Bool]]
          for each argument:
            call dfs on the argument to get the programs that create that argument
                                                e.g. Int becomes [f,g,h], Bool becomes []
          now you have a list of [RProgram] for each argument
          take our function F (e.g. h: Int -> Bool -> Int) and fill in the arguments using the [RProgram] we get
          to get a new list of [RProgram]       e.g. if F is g, then return [g f]
          we somehow take these programs for each argument and combine them into a bunch of larger programs
    then we return [RProgram] that build up the goalType
--}


    {--
        * get unified functions for original goal type
        * if any of those are ground
              return the first one that's ground
        * if neither are ground 
              look at the first unified function and recurse on its chiren 

                query: Bool -> Int
                                        goalType
              guard: whatever program we're looking at that's coming out of getUnifiedFunctions,
                     we want to check if it's ground          
          level 1:    find the ground program at this level, else           (incomplete: f (?? :: Bool) )     g ??? ??? ???? ???? ????        arg0
          level 2:    find the ground program at this level, else       
          level 3:    find the ground program at this level


    --}




    -- guard ()

  -- return undefined 


-- naiveFactorize' :: Int -> Logic (Int, Int)
-- naiveFactorize' n =
--     x <- nat
--     y <- nat
--     guard (2^x * y == n)
--     return (x, y)



  -- for each of these functions, find solutions
  -- functionSolutions <- mapM turnFunctionIntoSolutions unifiedFuncs :: TopDownBackTrack IO [RProgram] -- [solutions for func1, solutions for func2]
  -- let allFunctionSolutions = concat functionSolutions :: [RProgram]
  -- return allFunctionSolutions





--
-- gets list of components/functions that unify with a given type
-- 
-- getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> CompsSolver IO [(Id, SType)]


-- getUnifiedFunctions' :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT CheckerState IO [(Id, SType)]
-- getUnifiedFunctions' env messageChan ((id, schema) : ys) goalType = do
--     -- (id, schema) <- func
--     freshVars <- freshType (env ^. boundTypeVars) schema
--     -- liftIO $ putStrLn $ "Consumed a value: " ++ id ++ ", with goalType: " ++ (show goalType)

--     let t1 = shape (lastType freshVars) :: SType
--     let t2 = goalType :: SType

--     modify $ set isChecked True
--     modify $ set typeAssignment Map.empty

--     solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
--     st' <- get
    
--     -- (freshVars, st') <- do

--     --   return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

--     let sub =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     let schema' = stypeSubstitute sub (shape freshVars)

--     -- if it unifies, add that particular unified compoenent to state's list of components
--     if (checkResult) 
--       then fmap ((id, schema') :) (getUnifiedFunctions' env messageChan ys goalType)
--       else getUnifiedFunctions' env messageChan ys goalType 
--     -- guard checkResult
--     -- return (id, schema')




-- getUnifiedFunctions' :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> TopDownBackTrack IO (Id, SType) -> TopDownBackTrack IO (Id, SType)

-- -- MAYBE HOW TO DO THIS ????????????????????
-- -- existingList `mplus` (return singleElement) 
-- getUnifiedFunctions' _ _ [] _ unifiedFuncs = unifiedFuncs

-- getUnifiedFunctions' env messageChan ( v@(id, schema) : ys) goalType unifiedFuncs = do
--     (freshVars, st') <- lift $ do

--       freshVars <- freshType (env ^. boundTypeVars) schema
--       liftIO $ putStrLn $ "Consumed a value: " ++ id ++ ", with goalType: " ++ (show goalType)

--       let t1 = shape (lastType freshVars) :: SType
--       let t2 = goalType :: SType

--       modify $ set isChecked True
--       modify $ set typeAssignment Map.empty

--       solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
--       st' <- get
      
--       return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

--     let sub =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     let schema' = stypeSubstitute sub (shape freshVars)

--     -- if it unifies, add that particular unified compoenent to state's list of components
--     getUnifiedFunctions' env messageChan ys goalType $
--       if (checkResult) 
--         then unifiedFuncs `mplus` (return (id, schema'))
--         else unifiedFuncs









-- iterativeDeepening :: Environment -> Chan Message -> SearchParams -> [Example] -> RSchema -> IO ()
-- iterativeDeepening env messageChan searchParams examples goal = do
-- -- solution <- choices []
-- -- solution <- once $ iterativeDeepening 1 `mplus` iterativeDeepening 2 `mplus` ...
-- -- solution <- once $ msum [iterativeDeepening 1, iterativeDeepening 2]
-- -- solution <- once $ msum $ map iterativeDeepening [1..]
--   solution <- observeT $ msum $ map helper [1..] :: IO RProgram
--   print solution

--   where
--     helper :: Int -> LogicT IO RProgram
--     helper depth = do
--       liftIO $ printf "running dfs on %s at depth %d\n" (show goal) depth

--       let goalType = shape $ lastType $ toMonotype goal :: SType
--       solutions <- lift $ evalCompsSolver messageChan $ dfs env messageChan depth goalType :: LogicT IO [RProgram]

--       solutionLazy <- choices solutions
--       isChecked <- liftIO $ check' solutionLazy
--       guard isChecked -- gets the first valid program

--       -- liftIO $ print solutionLazy
--       return solutionLazy
--     check' :: RProgram -> IO Bool
--     check' program = do
--       -- printf "omg we are checking this program: %s\n" (show program)
--       checkResult <- evalStateT (check env searchParams examples program goal messageChan) emptyFilterState
--       case checkResult of
--         Nothing  -> return False
--         Just exs -> do
--           -- TODO add this back in
--           -- out <- toOutput env program exs
--           -- printResult $ encodeWithPrefix out
--           return True
    
--     -- converts [a] to a Logic a
--     choices :: MonadPlus m => [a] -> m a
--     choices = msum . map return


















-- type PNSolver m = StateT SolverState m
-- type BackTrack m = LogicT (PNSolver m)
-- type IncompletePrograms = [(Id, SType)]
-- type TopDownBackTrack m = StateT IncompletePrograms (LogicT (StateT CheckerState m))


-- type TopDownBackTrack m = LogicT (StateT IncompletePrograms (StateT CheckerState m))


-- -- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m [a]
-- -- evalTopDownBackTrack messageChan action = observeAllT action `evalStateT` (emptyChecker { _checkerChan = messageChan })
-- evalTopDownBackTrack :: Monad m => Chan Message -> TopDownBackTrack m a -> m a
-- evalTopDownBackTrack messageChan action = action'''
--   where
--     action' = observeT action
--     action'' = action' `evalStateT` []
--     action''' = action'' `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- type CompsSolver m = StateT Comps (StateT CheckerState m)


-- type TopDownBackTrack m = LogicT (StateT IncompletePrograms (StateT CheckerState m))

-- getUnifiedFunctions' :: Environment -> Chan Message -> TopDownBackTrack IO (Id, RSchema) -> SType -> TopDownBackTrack IO (Id, SType)

-- -- MAYBE HOW TO DO THIS ????????????????????
-- -- existingList `mplus` (return singleElement) 
-- -- getUnifiedFunctions' _ _ [] _ = mzero

-- getUnifiedFunctions' env messageChan func goalType = do
--     (id, schema) <- func
--     (freshVars, st') <- lift $ lift $ do

--       freshVars <- freshType (env ^. boundTypeVars) schema
--       -- liftIO $ putStrLn $ "Consumed a value: " ++ id ++ ", with goalType: " ++ (show goalType)

--       let t1 = shape (lastType freshVars) :: SType
--       let t2 = goalType :: SType

--       modify $ set isChecked True
--       modify $ set typeAssignment Map.empty

--       solveTypeConstraint env t1 t2 :: StateT CheckerState IO ()
--       st' <- get
      
--       return (freshVars, st') :: StateT CheckerState IO (RType, CheckerState)

--     let sub =  st' ^. typeAssignment
--     let checkResult = st' ^. isChecked

--     let schema' = stypeSubstitute sub (shape freshVars)

--     -- if it unifies, add that particular unified compoenent to state's list of components
--     -- getUnifiedFunctions' env messageChan ys goalType `mplus`
--     --   if (checkResult) 
--     --     then (return (id, schema'))
--     --     else mzero
--     guard checkResult
--     return (id, schema')












-- Main> runLogic (once $ myFunc 1 []) (\a r -> a:r) []
-- [[16,15,14,13,12,11,10]]


-- newtype LogicT m a =
--     LogicT { unLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }

--         runLogic :: Logic a -> (a -> r -> r) -> r -> r
--         runLogic l s f = runIdentity $ unLogicT l si fi
--         where
--         si = fmap . s
--         fi = Identity f

        -- Runs a Logic computation with the specified initial success and failure continuations.
    -- where
    --   -- determines if the result has all the appropriate arguments given the number of args
    --   -- TODO add "check" function here
    --   filterParams :: Int -> RProgram -> Bool
    --   filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
    --   filterParams 1       x = "arg0" `isInfixOf` (show x)
    --   filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) (show x) && filterParams (numArgs - 1) x


-- type CompsSolver m = StateT Comps (StateT CheckerState m)
-- evalCompsSolver messageChan m = m `evalStateT` emptyComps `evalStateT` (emptyChecker { _checkerChan = messageChan })

-- instance Monad m => CheckMonad (CompsSolver m) where
--     getNameCounter = lift getNameCounter
--     setNameCounter = lift . setNameCounter
--     getNameMapping = lift getNameMapping
--     setNameMapping = lift . setNameMapping
--     getIsChecked   = lift getIsChecked
--     setIsChecked   = lift . setIsChecked
--     getMessageChan = lift getMessageChan
--     overStats      = lift . overStats

--
-- does DFS stuff
--
-- Environment -> Int -> SType -> [RProgram]
-- type TopDownBackTrack m = LogicT (StateT IncompletePrograms (StateT CheckerState m))

-- we want to have the first level generate the second level
--   before processing the second level, e.g.
-- first level (goal is Int) is arg0, id, length
-- second level (goal is [a]) is concat, tail, Nil




---------------------
-- bfs :: Environment -> Chan Message -> Int -> SType -> TopDownBackTrack IO RProgram
-- bfs env messageChan depth goalType = do
  
--   -- collect all the component types (which we might use to fill the holes)
--   let components = Map.toList (env ^. symbols)

--   -- find all functions that unify with goal type
--   -- unifiedFuncs <- getUnifiedFunctions' env messageChan components goalType :: CompsSolver IO [(Id, SType)]

--   -- Int -> Int
--   -- unifiedFuncs should first return arg0
--   -- checks if that's ground and returns that
--   -- if it gets to something else, like `add`
--     -- then 

-- {-
--   (x:xs)
--     x is new program 
--     do stuff with x (decide if complete or not)
--       and check that it matches the examples
--         return if that's the case
--       if not ground, do the newFunc that we wrote
--           recurse with (xs ++ newStuff)
-- -}

--   -- unifiedFunc@(id, schema) <- getUnifiedFunctions' env messageChan components goalType mzero :: TopDownBackTrack IO (Id, SType)
--   -- unifiedFuncs <- lift $ getUnifiedFunctions' env messageChan components goalType :: TopDownBackTrack IO [(Id, SType)]
  
--   unifiedFuncs <- lift $ lift $ getUnifiedFunctions''' env messageChan components goalType :: TopDownBackTrack IO [RProgram]
  
--   program <- choices unifiedFuncs :: TopDownBackTrack IO RProgram

--   when (hasHole program) $ do
--     -- fillHoles :: Environment -> Chan Message -> RProgram -> StateT CheckerState IO [RProgram]
--     newHoledPrograms <- lift $ lift $ fillHoles env messageChan program :: TopDownBackTrack IO [RProgram]
--     st <- get
--     st `mplus` (choices newHoledPrograms)

--   guard (not $ hasHole program)
--   return program
  -- return Program { content = PSymbol id, typeOf = refineTop env schema }
----------------------














---------------
        -- TODO should we use `examples` or something else? also, `goalType` correct? 

        -- myCheck [RProgram] -> Maybe something THIS GOT DELETED AND I COULDN"T GET IT BACK 
        --     myCheck program = check envWithHo searchParams examples program goalType messageChan

        -- -- how to use FilterTest in BackTrack (from check in src/PetriNet/PNSolver.hs)
        -- (checkResult, fState') <- withTime TypeCheckTime $ 
        --     liftIO $ runStateT (check env params examples code' goal msgChan) fState

        -- -- this is how you evaluate BackTrack, in PNSolver
        -- searchResults <- withTime FormerTime $ observeManyT cnt $
        --     enumeratePath env goal examples usefulTrans

        -- this is how to use PNSolver TODO ask zheng


        -- type PNSolver m = StateT SolverState m
        -- type BackTrack m = LogicT (PNSolver m)

        -- check :: MonadIO m 
        --       => Environment -- symbol environment
        --       -> SearchParams -- search parameters: to control what to be checked
        --       -> [Example] -- examples for post-filtering
        --       -> RProgram -- program to be checked
        --       -> RSchema -- goal type to be checked against
        --       -> Chan Message -- message channel for logging
        --       -> FilterTest m (Maybe AssociativeExamples) -- return Nothing is check fails, otherwise return a list of updated examples
        -- check env searchParams examples program goalType solverChan =
        --     runGhcChecks searchParams env (lastType $ toMonotype goalType) examples program

        -- how to use FilterTest (from check in HooglePlus/GHCChecker.hs)
        -- (checkResult, fState') <- withTime TypeCheckTime $ 
        --     liftIO $ runStateT (check env params examples code' goal msgChan) fState

        -- how to use BackTrack (from findProgram in PetriNet/PNSolver.hs)
        -- searchResults <- withTime FormerTime $ observeManyT cnt $
        --     enumeratePath env goal examples usefulTrans

------------



