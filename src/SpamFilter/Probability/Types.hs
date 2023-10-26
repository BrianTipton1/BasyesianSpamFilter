{-# LANGUAGE GADTs #-}

module SpamFilter.Probability.Types where

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