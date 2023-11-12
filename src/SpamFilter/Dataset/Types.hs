{-# LANGUAGE GADTs #-}

module SpamFilter.Dataset.Types where

import Data.CSV.Types (CSV (CSV), Column (Column), Header (Header), Row (Row))
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

type Words = [String]

data Message where
    Spam :: Words -> Message
    Ham :: Words -> Message
    deriving (Show)

newtype Dataset where
    Dataset :: { theMessagesOf :: [Message] } -> Dataset
    deriving (Show)

class ToRow a where
    toRow :: a -> Row

class FromRow a where
    fromRow :: Row -> Maybe a

fromCSV :: FromRow a => CSV -> Maybe [a]
fromCSV (CSV (_, rows)) = mapM fromRow rows

toCSV :: ToRow a => Header -> [a] -> CSV
toCSV header messages = CSV (header, map toRow messages)
