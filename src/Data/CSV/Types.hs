{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Data.CSV.Types where

import Data.List (intercalate, sort)
import qualified Data.Map.Strict as Map

newtype Column where
    Column :: String -> Column
    deriving (Eq, Ord)

newtype Header where
    Header :: {getColumns :: [Column]} -> Header

newtype Row = Row (Header, Map.Map Column String)

newtype CSV = CSV (Header, [Row])


instance Show Column where
    show :: Column -> String
    show (Column str) = escapeAndQuote str

instance Show Header where
    show :: Header -> String
    show (Header cols) = intercalate "," (map show cols)

instance Show Row where
    show :: Row -> String
    show (Row (Header cols, m)) = 
        intercalate "," . map (\col -> escapeAndQuote (Map.findWithDefault "" col m)) $ cols


instance Show CSV where
    show :: CSV -> String
    show (CSV (header, rows)) = 
        show header ++ "\n" ++ intercalate "\n" (map (showWithHeader header) rows)

showWithHeader :: Header -> Row -> String
showWithHeader header (Row (_, m)) = 
    intercalate "," . map (\col -> escapeAndQuote (Map.findWithDefault "" col m)) $ getColumns header

escapeAndQuote :: String -> String
escapeAndQuote str = "\"" ++ concatMap (\c -> if c == '\"' then "\"\"" else [c]) str ++ "\""