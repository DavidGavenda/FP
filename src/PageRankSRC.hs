{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

-- https://www.geeksforgeeks.org/page-rank-algorithm-implementation/

module PageRankSRC where
    
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Graph.DGraph ( DGraph )
import Data.Graph.Types
    ( Graph (vertices, adjacentVertices, reachableAdjacentVertices) )
import Data.Ord ( comparing )
import Data.List ( sortBy, (\\) )


formatInput :: Int -> Map String Double -> Map String Double
formatInput n = Map.map (\x -> 1 / fromIntegral n)

reachableNodesCount :: DGraph String () -> Map String Double -> String -> Double
reachableNodesCount graph edges node = do
    let number = length (reachableAdjacentVertices graph node)
    let rank = edges ! node
    let total = rank / fromIntegral number
    total

isNodeReachable :: [String] -> [String] -> String -> DGraph String () -> [String]
isNodeReachable nodes nodesReach node graph = do
  let reachable = reachableAdjacentVertices graph (head nodes)
  if not (null nodes)
    then 
        if node `elem` reachable
            then isNodeReachable (drop 1 nodes) (nodesReach ++ [head nodes]) node graph
            else isNodeReachable (drop 1 nodes)  nodesReach node graph
    else do nodesReach

stringToTuple :: [String] -> [(String, Double)]
stringToTuple = map (\a -> (a, 0))

stringToMap :: [String] -> Map String Double
stringToMap = Map.fromList . stringToTuple

equals :: Map String Double -> Map String Double -> Bool
equals a b = do
    let listB = Map.toList b
    let listA = Map.toList a
    all (\(x, y) -> 10e-5 > abs (y - (b ! x))) listA
   
getCountPageRank :: Double -> Map String Double -> DGraph String () -> [String] -> Map String Double
getCountPageRank alpha edges graph nodes = do
    if not (null nodes)
    then
        let 
            url = head nodes
            neighbors = adjacentVertices graph url
            reachableNeighbors = reachableAdjacentVertices graph url
            source1 = neighbors \\ reachableNeighbors
            source2 = source1 ++ isNodeReachable reachableNeighbors [] url graph
            count = map (reachableNodesCount graph edges) source2
            totalCount = sum count

            newRank = (1-alpha) + (alpha * totalCount)
            newRankValues = Map.insert url newRank edges

        in getCountPageRank alpha newRankValues graph (drop 1 nodes)
    else edges

alpha :: Double
alpha = 0.85

pageRank :: Map String Double -> Map String Double -> DGraph String () -> Int -> Map String Double
pageRank oldValuesPR newValuesPR graph i = do
    if i <= 1
        then do
            let newPR = getCountPageRank alpha newValuesPR graph (vertices graph)
            pageRank newValuesPR newPR graph (i + 1)
        else do
            let correlation = equals oldValuesPR newValuesPR
            if not correlation
              then do
                let newPR = getCountPageRank alpha newValuesPR graph (vertices graph)
                pageRank newValuesPR newPR graph (i + 1)
            else newValuesPR
                
sortPageRank :: [(String, Double)] -> [(String, Double)]
sortPageRank = sortBy (comparing snd)

createJson :: [(String, Double)] -> IO ()
createJson (x:xs) = do
    let json = "{\"url\":\"" ++ fst x ++ "\",\"value\":" ++ show (snd x) ++ "}\n"
    appendFile "output.jsonl" json
    createJson xs
createJson [] = return ()
