module SpamFilter.Dataset.Util where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import SpamFilter.Dataset.Types (Dataset (Dataset), Message (Ham, Spam), Words)
import Util.String (split)

toDataset :: [String] -> Dataset
toDataset =
  Dataset
    . map
      ( \x ->
          if isHam x
            then Ham (rowToWords x)
            else Spam (rowToWords x)
      )
 where
  splitOnTab = split '\t'
  rowToWords = words . tail
  isHam s = head (splitOnTab s) == "ham"

messageWords :: Message -> [String]
messageWords (Spam words) = words
messageWords (Ham words) = words

countWords :: Foldable t => t Message -> Int
countWords = foldl' (\acc msg -> acc + length (messageWords msg)) 0

filterMessagesOf :: (Words -> Message) -> [Message] -> [Message]
filterMessagesOf t msgs =
  case t [] of
    Spam _ -> filter (not . isSpam) msgs
    _ -> filter isSpam msgs
 where
  isSpam (Spam _) = True
  isSpam _ = False

joinMaps :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int
joinMaps = Map.unionWith (+)

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
