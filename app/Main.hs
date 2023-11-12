module Main where
import SpamFilter.Dataset.Types (Dataset(..), Message (Ham, Spam))
import Util.Cli (getOps, addDefaultPath)
import SpamFilter.Dataset.Util (dataset, filterMessagesOf, countWords, buildMap, joinMaps)
import SpamFilter.Probability.Util (allProbabilities)
import SpamFilter.Probability.Types (probabilitesToCsv)
import Data.CSV.IO (writeCSV)


main :: IO ()
main = do
  ops <- getOps
  let path = addDefaultPath ops
  (Dataset messages) <- dataset path
  let spam = filterMessagesOf Ham messages
      ham = filterMessagesOf Spam messages

      nSpamWords = countWords spam
      nHamWords = countWords ham
      nTotalWords = nHamWords + nSpamWords

      spamTotals = buildMap spam
      hamTotals = buildMap ham
      allTotals = joinMaps spamTotals hamTotals

      allProbs = allProbabilities allTotals spamTotals hamTotals
      csv = probabilitesToCsv allProbs

  -- print allTotals
  -- print allProbs
  -- print csv
  writeCSV csv "test.csv"
