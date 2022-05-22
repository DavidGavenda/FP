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
    let emptyGraph = createEmptyDGraph
    (graph, resultIndex) <- createGraph file emptyGraph emptyMap
    let edges = stringToMap (Data.Graph.Types.vertices graph)
    let numberOfEdges = Data.Graph.Types.order graph
    let initialPageRankValues = formatInput numberOfEdges edges
    let finalPageRankValues = pageRank edges initialPageRankValues graph 1
    let sortedPageRank = sortPageRank (Map.toList finalPageRankValues)

    createJson sortedPageRank
    fileWriteMap resultIndex
    hClose file
