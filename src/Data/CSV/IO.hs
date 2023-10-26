module Data.CSV.IO where

import Data.CSV.Types (CSV (CSV), Row (Row))
import Data.CSV.Util (buildCSV, buildHeader, buildRow)
import qualified Data.Map.Strict as Map
import Util.String (split)

readCSV :: FilePath -> IO (Maybe CSV)
readCSV filePath = do
    content <- readFile filePath
    let linesOfFields = map (split ',') (lines content)
    let header = buildHeader (head linesOfFields)
    let numOfColumns = length (head linesOfFields)
    let rowsData = tail linesOfFields
    if all (\row -> length row == numOfColumns) rowsData
        then do
            let rows = map (buildRow header) rowsData
            return $ Just $ CSV rows
        else return Nothing

writeCSV :: CSV -> FilePath -> IO ()
writeCSV csv filePath = do
    let csvString = show csv
    writeFile filePath csvString

appendToCSV :: FilePath -> Row -> IO (Maybe CSV)
appendToCSV filePath newRow = do
    content <- readFile filePath
    let linesOfFields = map (split ',') (lines content)
    let header = buildHeader (head linesOfFields)
    let numOfColumns = length (head linesOfFields)
    let existingRowsData = tail linesOfFields
    let newRowCount = length $ getRowItems newRow
    if newRowCount == numOfColumns
        then do
            let updatedRowsData = existingRowsData ++ [getRowItems newRow]
            let maybeUpdatedCSV = buildCSV header updatedRowsData
            case maybeUpdatedCSV of
                Just updatedCSV -> do
                    writeFile filePath (show updatedCSV)
                    return $ Just updatedCSV
                Nothing -> return Nothing
        else return Nothing
  where
    getRowItems :: Row -> [String]
    getRowItems (Row items) = map snd (Map.toList items)