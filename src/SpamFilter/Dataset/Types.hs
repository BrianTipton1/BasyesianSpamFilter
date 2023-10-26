{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module SpamFilter.Dataset.Types where

import Data.CSV.Types (CSV (CSV), Column (Column), Row (Row))
import qualified Data.Map.Strict as Map
import SpamFilter.Probability.Types (WordProbability (WordProbability, nHam, nSpam, nTotal, probExists, probHam, probSpam, word))
import Text.Read (readMaybe)

type Words = [String]

data Message where
    Spam :: Words -> Message
    Ham :: Words -> Message
    deriving (Show)

newtype Dataset where
    Dataset :: {theMessagesOf :: [Message]} -> Dataset
    deriving (Show)

class ToRow a where
    toRow :: a -> Row

instance ToRow WordProbability where
    toRow :: WordProbability -> Row
    toRow wordProb = Row dataMap
      where
        columns =
            [ Column "Word"
            , Column "ProbHam"
            , Column "ProbSpam"
            , Column "ProbExists"
            , Column "NHam"
            , Column "NSpam"
            , Column "NTotal"
            ]
        values =
            [ word wordProb
            , show (probHam wordProb)
            , show (probSpam wordProb)
            , show (probExists wordProb)
            , show (nHam wordProb)
            , show (nSpam wordProb)
            , show (nTotal wordProb)
            ]
        dataMap = Map.fromList $ zip columns values

class FromRow a where
    fromRow :: Row -> Maybe a

instance FromRow WordProbability where
    fromRow :: Row -> Maybe WordProbability
    fromRow (Row rowMap) =
        WordProbability
            <$> Map.lookup (Column "Word") rowMap
            <*> (Map.lookup (Column "ProbHam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "ProbSpam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "ProbExists") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "NHam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "NSpam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "NTotal") rowMap >>= readMaybe)

fromCSV :: FromRow a => CSV -> Maybe [a]
fromCSV (CSV rows) = mapM fromRow rows

toCSV :: ToRow a => [a] -> CSV
toCSV = CSV . map toRow
