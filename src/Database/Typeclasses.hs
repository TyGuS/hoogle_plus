module Database.Typeclasses where

    import Types.Environment
    import Types.Program
    import Types.Type
    import qualified Data.Map as Map

    -- new data decalrations
    -- TODO: move to Types after done w/ design
    -- TODO: derive Show + what else? Eq, Ord appear throughout the codebase
    data Typeclass = Typeclass {
        dictName :: String,
        params :: [String],
        interface :: [(String, (TypeSkeleton a))] -- TODO: can't compile due to `out of scope a`, but why?
    }

    data DataSig = DataSig -- TODO: I'm unsure about this one
    data FunctionSig = FunctionSig -- TODO: Isn't this FunctionT (?)

    -- new function declarations
    -- TODO: determine where these should be located
    -- The `Pos` stuff is really useless isn't it?
    mkTypeclassDict :: Environment -> Typeclass -> ConstructorSig
    mkTypeclassDict env tyclass = ConstructorSig tyclassName tySig
        where tySig = foldl toSignature returnTy $ interface
              tyclassName = dictName tyclass

              -- TODO: does the function name matter? 
              toSignature acc arg = Fun "_" $ arg acc

              -- TODO: it is not necessarily true that the interface contains only functions?
              -- I believe it may contain superclass dictionaries. But maybe those behave as
              -- functions too?
              interfaceAsArgs = map processInterface $ interface tyclass
              processInterface (fname, fsig) = (FunctionT fname fsig)

              -- to get typeclass params
              annotateParams x = ScalarT (TypeVarT Map.empty x) ftrue
              params = map annotateParams $ params tyclass

              -- final signature
              returnTy = (ScalarT (DatatypeT tyclassName params []) ftrue)



    unTypeclass :: Environment -> FunctionSig -> ConstructorSig
    unTypeclass env funcSig = undefined

    mkTypeclassInstance :: Environment -> Typeclass -> DataSig -> ConstructorSig
    mkTypeclassInstance env typeclass dataSig = undefined