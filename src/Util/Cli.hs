module Util.Cli where

import Data.List (isSubsequenceOf, find)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Directory (doesFileExist)
import System.Exit (exitWith)

data CommandLineOption
    = CmdPath String
    | CmdHelp
    | CmdReport
    deriving (Eq, Show)

cmdHelpList :: [String]
cmdHelpList = ["-h", "--help"]
cmdReport :: [String]
cmdReport = ["-r", "--report"]

checkOps :: [CommandLineOption] -> IO [CommandLineOption]
checkOps ops
    | containsHelp ops
        || length (filter isCmdPath ops) > 1 =
        do
            putStrLn help
            exitWith $ ExitFailure 1
    | otherwise = return ops
  where
    isCmdPath (CmdPath _) = True
    isCmdPath _ = False
    containsHelp = elem CmdHelp

addDefaultPath :: [CommandLineOption] ->  String
addDefaultPath ops =
    case opsToPath ops of
        Just (CmdPath path) ->  path
        otherwise -> "SMSSpamCollection.txt"

strToOps :: String -> IO CommandLineOption
strToOps s
    | s `elem` cmdHelpList = return CmdHelp
    | s `elem` cmdReport = return CmdReport
    | otherwise = do
        fe <- doesFileExist s
        if fe
            then return $ CmdPath s
            else return CmdHelp

help :: String
help =
    unlines
        [ "Usage: BayesianSpamFilter [OPTIONS] [PATH]"
        , ""
        , "Options:"
        , "  -r, --report           Generate a report on the data processed (requires pdflatex)."
        , "  -h, --help             Show this help message and exit."
        , ""
        , "Arguments:"
        , "  PATH                   The path to the dataset file containing the text message data to be processed."
        , "                         Default: $PWD/SMSSpamCollection.txt"
        , ""
        , "Description:"
        , "  This CLI application utilizes Bayesian filtering techniques to categorize"
        , "  text messages as spam or ham (not-spam). It processes text message data"
        , "  from the specified dataset file or from $PWD/SMSSpamCollection.txt if no"
        , "  path is given."
        , ""
        , "  The --report flag can be used to generate a report detailing the"
        , "  classification results, including information such as the number"
        , "  of messages processed, the number classified as spam, and the overall"
        , "  accuracy of the filter. The report is generated using the pdflatex command."
        , ""
        , "  Significant results will be output to the terminal screen with or without the --report flag."
        , ""
        , "Examples:"
        , "  # Process text message data from the default dataset file and generate a report"
        , "  $ BayesianSpamFilter --report"
        , ""
        , "  # Process text message data from the specified dataset file"
        , "  $ BayesianSpamFilter /path/to/SMSSpamCollection.txt"
        , ""
        , "  # Process text message data from the specified dataset file and generate a report"
        , "  $ BayesianSpamFilter --report /path/to/SMSSpamCollection.txt"
        ]

opsToPath :: [CommandLineOption] -> Maybe CommandLineOption
opsToPath = find isCmdPath
  where
    isCmdPath (CmdPath _) = True
    isCmdPath _           = False
