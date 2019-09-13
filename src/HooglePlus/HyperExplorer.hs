module HooglePlus.HyperExplorer where

import Types.HyperGraph
import Types.Abstract

hyperSearch :: Graph AbstractSkeleton -> [Vertex AbstractSkeleton]
hyperSearch = undefined

bfs :: Graph AbstractSkeleton -> AbstractSkeleton -> [Vertex AbstractSkeleton] -> [Vertex AbstractSkeleton]
bfs g target [] = []
bfs g target (curr:q) =
    if eLabel == target
        then let p = getPath curr [] in if validatePath p then p else enqueue
        else enqueue
    where
        eLabel = vertexLabel curr
        eChildren = vertexChildren curr
        eParens = vertexParents curr
        eTruth = vertexTruth curr
        ePrev = vertexPrev curr

        updateAnds v = case vertexType v of
            AndNode -> v { vertexTruth = curr : vertexTruth v }
            _ -> v

        setPrev v = v { vertexPrev = curr : vertexPrev v }

        enqueue = case vertexType curr of
            AndNode -> if all (`elem` map vertexLabel ePrev) (map vertexLabel eParens)
                            then bfs g (q ++ map setPrev eChildren)
                            else bfs g (q ++ [curr])
            _ -> bfs g (q ++ map (setPrev . updateAnds) eChildren)

        getPath v res | null (vertexParents v) = v : res
                      | otherwise = foldr (\ver acc -> getPath ver acc) (v:res) (vertexPrev v)


        validateOrNode v | OrNode <- vertexType v =
            let indegree = length (filter ((`elem` p) . vertexLabel) (vertexParents v))
                outdegree =  length (filter ((`elem` p) . vertexLabel) (vertexChildren v))
            in indegree <= outdegree + 1
        validateOrNode _ = True

        validatePath p = all (\v -> any ((`elem` p) . vertexLabel) (vertexChildren v)) eTruth &&
                         all validateOrNode eTruth