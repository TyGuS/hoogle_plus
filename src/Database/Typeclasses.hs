module Database.Typeclasses where

    import Types.Environment
    import Types.Program
    import Types.Type
    import qualified Data.Map as Map
    import Synquid.Logic

    -- new data decalrations
    -- TODO: move to Types after done w/ design
    -- TODO: derive Show + what else? Eq, Ord appear throughout the codebase
    data Typeclass = Typeclass {
        dictName :: String,
        params :: [String],
        interface :: [(TypeSkeleton Formula)]
    }

    data DataSig = DataSig -- TODO: Isn't this DatatypeT (?)
    data FunctionSig = FunctionSig -- TODO: Isn't this FunctionT (?) or maybe BareDeclaration (FuncDecl)

    -- new function declarations
    -- TODO: determine where these should be located
    -- The `Pos` stuff is really useless isn't it?
    -- TODO: I'm not using the environment at all
    mkTypeclassDict :: Environment -> Typeclass -> ConstructorSig
    mkTypeclassDict env tyclass = ConstructorSig tyclassName tySig
        where tySig = foldr toSignature returnTy $ interface tyclass
              tyclassName = dictName tyclass

              -- TODO: does the function name matter? If so, I can create freshvars
              toSignature acc arg = FunctionT "_" arg acc

              -- to get typeclass params
              annotateParams x = ScalarT (TypeVarT Map.empty "") ftrue
              tyclassParams = map annotateParams $ params tyclass

              -- final signature
              returnTy =  (ScalarT (DatatypeT tyclassName tyclassParams []) ftrue)



    unTypeclass :: Environment -> FunctionSig -> ConstructorSig
    unTypeclass env funcSig = undefined

    mkTypeclassInstance :: Environment -> Typeclass -> DataSig -> ConstructorSig
    mkTypeclassInstance env typeclass dataSig = undefined