module SpamFilter.Probability.Util where

import SpamFilter.Probability.Types (word, WordProbability (wordGivenHam, wordGivenSpam, probOverall, hamOccurences, spamOccurences, nTotal, WordProbability))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
wordProb :: String -> Map.Map String Int -> Map.Map String Int -> WordProbability
wordProb word spam ham =
  WordProbability
    { word = word
    , wordGivenHam = hamProb
    , wordGivenSpam = spamProb
    , probOverall = overallProb
    , hamOccurences = hamOccurences
    , spamOccurences = spamOccurences
    , nTotal = hamOccurences + spamOccurences
    }
 where
  hamOccurences = fromMaybe 0 (Map.lookup word ham)
  spamOccurences = fromMaybe 0 (Map.lookup word spam)

  hamProb = hamOccurences % numTotal
  spamProb = spamOccurences % numTotal
  overallProb = (hamOccurences + spamOccurences) % numTotal

  numSpam = Map.foldl' (+) 0 spam
  numHam = Map.foldl' (+) 0 ham
  numTotal = numHam + numSpam

allProbabilities :: Map.Map String a -> Map.Map String Int -> Map.Map String Int -> [WordProbability]
allProbabilities allTotals spamTotals hamTotals = 
  map (\x -> wordProb x spamTotals hamTotals) (Map.keys allTotals)
