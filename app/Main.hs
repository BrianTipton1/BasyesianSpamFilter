module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import SpamFilter.Dataset.Types (Dataset (Dataset), Message (Ham, Spam))
import SpamFilter.Dataset.Util (countWords, filterMessagesOf, joinMaps, messageWords, toDataset)
import System.Environment (getArgs)
import Util.Cli (addDefaultPath, checkOps, strToOps)
import Util.String (split)

dataset :: String -> IO Dataset
dataset path = toDataset <$> rawMessages path
 where
  rawMessages :: String -> IO [String]
  rawMessages path = lines <$> readFile path

buildMap :: [Message] -> Map.Map String Int
buildMap messages = foldl' addWord Map.empty (concatMap messageWords messages)
 where
  addWord :: Map.Map String Int -> String -> Map.Map String Int
  addWord map word = Map.insertWith (+) word 1 map

main :: IO ()
main = do
  ops <-
    getArgs
      >>= mapM strToOps
      >>= checkOps
  let path = addDefaultPath ops
  (Dataset messages) <- dataset path
  let spam = filterMessagesOf Ham messages
      ham = filterMessagesOf Spam messages

      nSpamWords = countWords spam
      nHamWords = countWords ham
      nTotalWords = nHamWords + nSpamWords

      spamTotals = buildMap spam
      hamTotals = buildMap ham
      allTotals = joinMaps spamTotals hamTotals

  print allTotals
