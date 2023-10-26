{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Data.CSV.Types where

import qualified Data.Map.Strict as Map

import Data.List (intercalate)

newtype Column where
    Column :: String -> Column
    deriving (Eq, Ord)

newtype Header where
    Header :: [Column] -> Header

newtype Row where
    Row :: Map.Map Column String -> Row

newtype CSV where
    CSV :: [Row] -> CSV

instance Show Column where
    show :: Column -> String
    show (Column str) = str

instance Show Header where
    show :: Header -> String
    show (Header cols) = intercalate "," (map show cols)

instance Show Row where
    show :: Row -> String
    show (Row m) = intercalate "," (map (\(Column col, val) -> val) (Map.toList m))

instance Show CSV where
    show :: CSV -> String
    show (CSV rows) = intercalate "\n" (map show rows)
