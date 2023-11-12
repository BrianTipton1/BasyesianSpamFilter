module Data.CSV.IO where

import Data.CSV.Types (CSV (CSV), Header, Row (Row))
import Data.CSV.Util (buildCSV, buildHeader, buildRow)
import qualified Data.Map.Strict as Map
import Util.String (split)

readCSV :: FilePath -> IO (Maybe CSV)
readCSV filePath = do
    content <- readFile filePath
    let linesOfFields = map (split ',') (lines content)
    let header = buildHeader (head linesOfFields)
    let rowsData = map (map unescapeCSV) (tail linesOfFields)
    return $ buildCSV header rowsData

unescapeCSV :: String -> String
unescapeCSV [] = []
unescapeCSV ('\\' : c : cs) = c : unescapeCSV cs
unescapeCSV ('"' : '"' : cs) = '"' : unescapeCSV cs
unescapeCSV (c : cs) = c : unescapeCSV cs

writeCSV :: CSV -> FilePath -> IO ()
writeCSV (CSV (header, rows)) filePath = do
    let csvString = show header ++ "\n" ++ unlines (map show rows)
    writeFile filePath csvString
