module Util.String where

split :: Char -> String -> [String]
split _ "" = [""]
split delimiter (x : xs)
  | x == delimiter = "" : rest
  | otherwise = (x : head rest) : tail rest
 where
  rest = split delimiter xs