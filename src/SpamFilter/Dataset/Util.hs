module SpamFilter.Dataset.Util where

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

filterMessagesOf :: (Words -> Message) -> [Message] -> [Message]
filterMessagesOf t msgs =
  case t [] of
    Spam _ -> filter (not . isSpam) msgs
    _ -> filter isSpam msgs
 where
  isSpam (Spam _) = True
  isSpam _ = False