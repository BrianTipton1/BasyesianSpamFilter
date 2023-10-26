{-# LANGUAGE GADTs #-}

module SpamFilter.Dataset.Types where

type Words = [String]

data Message where
    Spam :: Words -> Message
    Ham :: Words -> Message
    deriving (Show)

newtype Dataset where
    Dataset :: {theMessagesOf :: [Message]} -> Dataset
    deriving (Show)