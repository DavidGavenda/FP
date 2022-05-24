{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IndexerSRC where

import System.IO ()
import qualified Data.Map as Map
import Data.Graph.Types ( Graph (empty, insertEdgePairs) )
import Data.Graph.DGraph ( DGraph )

createEmptyDGraph :: DGraph String ()
createEmptyDGraph = insertEdgePairs [] empty

headList :: [a] -> a
headList (x:_) = x

tailList :: [a] -> [a]
tailList (_:xs) = xs

insertElemToMap :: Map.Map String [String] -> [String] -> [String] -> Map.Map String [String]
insertElemToMap map word list
  | null list = map
  | otherwise = do
    insertElemToMap (Map.insertWith (++) head word map) word tail
    where
      head = headList list
      tail = tailList list
  
fileWriteMap :: Map.Map String [String] -> IO ()
fileWriteMap mapVar = do
  let file = "indexer.jsonl"
  let mapVarStr = Map.toList mapVar
  let string = (concat . map (\(x, y) -> "{\"word\":\"" ++ x ++ "\",\"urls\":[\"" ++ unwords y ++ "\"]}\n")) mapVarStr
  writeFile file string
