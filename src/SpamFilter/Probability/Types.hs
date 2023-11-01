{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module SpamFilter.Probability.Types where

import Data.CSV.Types (Column (Column), Row (Row))
import qualified Data.Map.Strict as Map
import SpamFilter.Dataset.Types (FromRow (fromRow), ToRow (toRow))
import Text.Read (readMaybe)

data WordProbability where
    WordProbability ::
        { word :: String
        , probHam :: Rational
        , probSpam :: Rational
        , probExists :: Rational
        , nHam :: Integer
        , nSpam :: Integer
        , nTotal :: Integer
        } ->
        WordProbability

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
