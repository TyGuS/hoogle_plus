module Datalog.Souffle where

import PetriNet.Util

runSouffle :: Environment -> RSchema -> [Example] -> IO ()
runSouffle env goal examples = do
    let dst = lastType (toMonotype goal)
    paths <- findPath env dst
    observeT $ msum $ map (enumeratePath env goal examples) paths

enumeratePath :: Environment -> RSchema -> [Example] -> UProgram -> LogicT IO ()
enumeratePath env goal examples prog = do
    let gm = env ^. symbolGroups
    let getFuncs p = Map.findWithDefault Set.empty (getGroup p) gm
    let syms = Set.toList (symbolsOf prog)
    let allPaths = map getFuncs syms
    msum $ map (checkPath env goal examples prog syms) (sequence allPaths)

checkPath :: Environment -> RSchema -> [Example] -> UProgram -> [Id] -> LogicT IO ()
checkPath env goal examples prog syms = do
    -- ensure the usage of all the higher order arguments
    let hoArgs = Map.keys $ Map.filter (isFunctionType . toMonotype) (env ^. arguments)
    let getRealName = replaceId hoPostfix ""
    let filterPaths p = all (`elem` map getRealName p) hoArgs
    guard (filterPaths path)

    -- fill the sketch with the functions in the path
    codeResult <- fillSketch env path
    let dst = lastType (toMonotype goal)
    checkResult <- parseAndCheck env dst codeResult
    case checkResult of
        Left err -> mzero
        Right code -> checkSolution env goal examples code

checkSolution :: Environment -> RSchema -> [Example] -> RProgram -> LogicT IO SearchResult
checkSolution env goal examples code = do
    (checkResult, fState') <- 
        liftIO $ runStateT (check env params examples code' goal msgChan) fState
    let exs = fromJust checkResult
    out <- liftIO $ toOutput env code' exs
    lift $ writeSolution out
    return $ Found (code', exs)

fillSketch :: Environment -> [Id] -> LogicT IO String
fillSketch env firedTrans = do
    let args = Map.keys $ foArgsOf env
    let sigs = substPair $ substName firedTrans $ map (findFunction fm) reps
    writeLog 1 "fillSketch" $ text "found filtered sigs" <+> pretty sigs
    let initialFormer = FormerState HashMap.empty []
    progSet <- withTime FormerTime $ generateCode initialFormer env src args sigs
    let progList = sortOn (Data.Ord.Down . length) $ Set.toList progSet
    msum $ map return progList
    where
        substPair [] = []
        substPair (x:xs) 
            | pairProj `isPrefixOf` funName x =
                ( x { funName = replaceId pairProj "fst" (funName x), funReturn = [head (funReturn x)] } )
              : ( x { funName = replaceId pairProj "snd" (funName x), funReturn = [funReturn x !! 1] } )
              : substPair xs
            | otherwise = x : substPair xs

generateCode :: (ConstraintEncoder enc, MonadIO m)
             => FormerState
             -> Environment
             -> [AbstractSkeleton]
             -> [Id]
             -> [FunctionCode]
             -> BackTrack enc m (Set String)
generateCode initialFormer env src args sigs = do
    tgt <- gets $ view (refineState . targetType)
    cover <- gets $ view (refineState . abstractionCover)
    disrel <- getExperiment disableRelevancy
    let bound = env ^. boundTypeVars
    let rets = filter (isSubtypeOf bound tgt) (allTypesOf cover)
    writeLog 1 "generateCode" $ pretty src
    writeLog 1 "generateCode" $ pretty rets
    liftIO (evalStateT (generateProgram sigs src args rets disrel) initialFormer)
