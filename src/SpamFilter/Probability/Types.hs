{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module SpamFilter.Probability.Types where

import Data.CSV.Types (CSV (CSV), Column (Column), Header (Header, getColumns), Row (Row))
import qualified Data.Map.Strict as Map
import Data.Ratio (Ratio)
import SpamFilter.Dataset.Types (FromRow (fromRow), ToRow (toRow), toCSV)
import Text.Read (readMaybe)

data WordProbability where
    WordProbability ::
        { word :: String
        , wordGivenHam :: Ratio Int
        , wordGivenSpam :: Ratio Int
        , probOverall :: Ratio Int
        , hamOccurences :: Int
        , spamOccurences :: Int
        , nTotal :: Int
        } ->
        WordProbability
    deriving (Show)

header :: Header
header =
    Header
        [ Column "HamOccurences"
        , Column "NTotal"
        , Column "ProbOverall"
        , Column "SpamOccurences"
        , Column "Word"
        , Column "WordGivenHam"
        , Column "WordGivenSpam"
        ]

instance ToRow WordProbability where
    toRow :: WordProbability -> Row
    toRow wordProb = Row dataMap
      where
        columns = getColumns header
        values =
            [ show (hamOccurences wordProb)
            , show (nTotal wordProb)
            , show (probOverall wordProb)
            , show (spamOccurences wordProb)
            , word wordProb
            , show (wordGivenHam wordProb)
            , show (wordGivenSpam wordProb)
            ]
        dataMap = Map.fromList $ zip columns values

instance FromRow WordProbability where
    fromRow :: Row -> Maybe WordProbability
    fromRow (Row rowMap) =
        WordProbability
            <$> (Map.lookup (Column "Word") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "WordGivenHam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "WordGivenSpam") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "ProbOverall") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "HamOccurences") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "SpamOccurences") rowMap >>= readMaybe)
            <*> (Map.lookup (Column "NTotal") rowMap >>= readMaybe)

probabilitesToCsv :: [WordProbability] -> CSV
probabilitesToCsv = toCSV header

csvToProbabilities :: CSV -> Maybe [WordProbability]
csvToProbabilities (CSV (_, rows)) = mapM fromRow rows