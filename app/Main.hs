{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}


module Main where

import PageRankSRC (
    createJson,
    stringToMap,
    sortPageRank,
    pageRank,
    formatInput
    )
import ParserSRC ( createGraph )
import IndexerSRC ( fileWriteMap, createEmptyDGraph )
import System.IO ( utf8, hClose, openFile, IOMode(ReadMode) )
import Data.Graph.Types
    ( Graph
        (
        vertices, 
        insertEdgePairs, 
        empty, 
        order
        ) 
    )
import Data.Graph.DGraph ( DGraph )
import Data.Map (Map, (!))
import qualified Data.Map as Map
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )

main :: IO ()
main = do
    setLocaleEncoding utf8
    file <- openFile "smallCollection.jl" ReadMode
    let emptyMap = Map.empty
    print "Task 1/10 done"
    let emptyGraph = createEmptyDGraph
    print "Task 2/10 done"
    (graph, resultIndex) <- createGraph file emptyGraph emptyMap
    print "Task 3/10 done"
    let edges = stringToMap (Data.Graph.Types.vertices graph)
    print "Task 4/10 done"
    let numberOfEdges = Data.Graph.Types.order graph
    print "Task 5/10 done"
    let initialPageRankValues = formatInput numberOfEdges edges
    print "Task 6/10 done"
    let finalPageRankValues = pageRank edges initialPageRankValues graph 1
    print "Task 7/10 done"
    let sortedPageRank = sortPageRank (Map.toList finalPageRankValues)
    print "Task 8/10 done"
    --createJson sortedPageRank
    --print "Task 9/10 done"
    fileWriteMap resultIndex
    print "Task 10/10 done"
    hClose file
