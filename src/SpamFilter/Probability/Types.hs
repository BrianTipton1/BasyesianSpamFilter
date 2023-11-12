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
        [ Column "Word"
        , Column "WordGivenHam"
        , Column "WordGivenSpam"
        , Column "ProbOverall"
        , Column "HamOccurences"
        , Column "SpamOccurences"
        , Column "NTotal"
        ]

instance ToRow WordProbability where
    toRow :: WordProbability -> Row
    toRow wordProb = Row (header, dataMap)
      where
        columns = getColumns header
        values =
            [ word wordProb
            , show (wordGivenHam wordProb)
            , show (wordGivenSpam wordProb)
            , show (probOverall wordProb)
            , show (hamOccurences wordProb)
            , show (spamOccurences wordProb)
            , show (nTotal wordProb)
            ]
        dataMap = Map.fromList $ zip columns values

instance FromRow WordProbability where
    fromRow :: Row -> Maybe WordProbability
    fromRow (Row (_, rowMap)) =
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