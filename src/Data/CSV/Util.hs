module Data.CSV.Util where

import Data.CSV.Types (CSV (CSV), Column (Column), Header (Header), Row (Row))
import qualified Data.Map.Strict as Map

buildRow :: Header -> [String] -> Row
buildRow header@(Header columns) values = Row (header, items)
  where
    zipped = zip columns values
    items = Map.fromList zipped

buildHeader :: [String] -> Header
buildHeader strs = Header $ map Column strs

buildCSV :: Header -> [[String]] -> Maybe CSV
buildCSV header@(Header columns) rowsData =
    if all (\row -> length row == numOfColumns) rowsData
        then Just $ CSV (header, map (buildRow header) rowsData)
        else Nothing
  where
    numOfColumns = length columns
