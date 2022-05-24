{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ParserSRC where

import IndexerSRC ( insertElemToMap )

import Data.Aeson ( FromJSON, decode )
import Data.ByteString as S (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Lazy.Internal as L ( ByteString, packChars )
import Data.Text.ICU.Char ( property, Bool_(Diacritic) )
import Data.Text.ICU.Normalize
    ( normalize, NormalizationMode(NFD) )
import Data.Char ( isUpper )
import Data.Graph.DGraph ( DGraph )
import Data.Graph.Types ( Graph(insertEdgePairs) )
import Data.Graph.Visualize ()
import Data.Maybe ()
import Data.List ( isPrefixOf, nub, sort )
import Data.Graph.Connectivity ()
import Data.Ord ()
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Text as T

import GHC.Generics ( Generic )
import System.IO ( Handle, hIsEOF, hGetLine )
import Text.HandsomeSoup ( (!), css )
import Text.Regex.TDFA ()
import Text.XML.HXT.Core
import Zenacy.HTML ()


data WebPage = WebPage
  { url :: String,
    html_content :: String
  }
  deriving (Generic)
instance FromJSON WebPage

stringToByteString :: String -> L.ByteString
stringToByteString = L.packChars

removeDiacritics :: String -> String
removeDiacritics s = T.unpack noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD (T.pack s)

sortList :: [String] -> [String]
sortList = sort

-- https://stackoverflow.com/questions/10414861/removing-every-instance-of-the-empty-list-from-a-list-of-list
removeEmptyList :: [String] -> [String]
removeEmptyList = filter (not . null)

-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/src/XMonad.Prompt.html#deleteAllDuplicates
removeDuplicates :: [String] -> [String]
removeDuplicates = nub

-- #https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitIntoWords :: [Char] -> [[Char]]
splitIntoWords s = case dropWhile Char.isSpace s of
  "" -> []
  s' -> w : splitIntoWords s''
    where (w, s'') = break Char.isSpace s'

-- https://stackoverflow.com/questions/3373327/stripping-newlines-in-haskell
removeNewlines :: [Char] -> [Char]
removeNewlines = reverse . dropWhile (=='\n') . reverse

-- https://stackoverflow.com/questions/30242668/remove-characters-from-string-in-haskell
removeSpecialChars :: String -> String
removeSpecialChars = filter (\x -> x `elem` [' '] ++ ['a'..'z'] ++ ['A'..'Z'])

removeUpperLetters :: [[Char]] -> [[Char]]
removeUpperLetters = filter (not . any isUpper)

cleanUrl :: String -> [String] -> [(String, String)]
cleanUrl url x = [(url, x) | x <- x, "http://" `isPrefixOf` x || "https://" `isPrefixOf` x]

cleanBody :: [String] -> [String]
cleanBody x = [x | x <- x, not ("{{" `isPrefixOf` x), not (" {{" `isPrefixOf` x)]

insertEdge :: DGraph String () -> [(String, String)] -> DGraph String ()
insertEdge graph x = insertEdgePairs x graph

urlToIOStr :: WebPage -> IO [String]
urlToIOStr m = do
  return [url m]

--https://github.com/egonSchiele/HandsomeSoup
createGraph :: Handle -> DGraph String () -> Map.Map String [String] -> IO (DGraph String (), Map.Map String [String])
createGraph input graph mapVar =
  do
    line <- hIsEOF input
    if line
      then
        return (graph, mapVar)
      else do
        jsonLine <- hGetLine input
        let lineByteStr = stringToByteString jsonLine
        let mm = decode lineByteStr :: Maybe WebPage
        case mm of
          Just m -> do
            let docUrl = readString [withParseHTML yes, withWarnings no] (html_content m)
            links <- runX $ docUrl >>> css "a" ! "href"
            let removedDuplicates = removeDuplicates links
            let cleanUrls = cleanUrl (url m) removedDuplicates
            
            let docHtml = readString [withParseHTML yes, withWarnings no, withInputEncoding isoLatin1] (html_content m)
            html <- runX $ docHtml >>> css "body" //> neg (css "script") //> getText
            let removedDiacritics = unlines (map removeDiacritics (cleanBody html))
            let removedNewLines = removeNewlines removedDiacritics
            let removedSpecials = removeSpecialChars removedNewLines
            let splitWords = splitIntoWords removedSpecials
            let removedEmpty = removeEmptyList splitWords
            let removedUpper = removeUpperLetters removedEmpty
            let removedDuplicates = removeDuplicates removedUpper
            let sortedHtml = sortList removedDuplicates

            url <- urlToIOStr m
            print url
            let newGraph = insertEdge graph cleanUrls
            let newMap = insertElemToMap mapVar url sortedHtml
            createGraph input newGraph newMap
