module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import SpamFilter.Dataset.Types (Dataset (Dataset), Message (Ham, Spam))
import SpamFilter.Dataset.Util (filterMessagesOf, messageWords, toDataset)
import System.Environment (getArgs)
import Util.String (split)

filePath :: IO [Char]
filePath =
  getArgs
    >>= ( \x ->
            if null x
              then return "SMSSpamCollection.txt"
              else return (head x)
        )

dataset :: IO Dataset
dataset = toDataset <$> (filePath >>= rawMessages)
 where
  rawMessages :: String -> IO [String]
  rawMessages path = lines <$> readFile path

buildMap :: Map.Map String Int -> [Message] -> Map.Map String Int
buildMap acc messages = foldl' addWord acc (concatMap messageWords messages)
 where
  addWord :: Map.Map String Int -> String -> Map.Map String Int
  addWord map word = Map.insertWith (+) word 1 map

main :: IO ()
main = do
  (Dataset messages) <- dataset
  let spam = filterMessagesOf Ham messages
      ham = filterMessagesOf Spam messages

      nSpamWords = foldl' (\acc msg -> acc + length (messageWords msg)) 0 spam
      nHamWords = foldl' (\acc msg -> acc + length (messageWords msg)) 0 ham
      nTotalWords = nHamWords + nSpamWords

      spamTotals = buildMap Map.empty spam
      hamTotals = buildMap Map.empty ham

  print ""
