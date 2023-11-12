module Util.Tex where

import qualified SpamFilter.Probability.Basyesian as Bayes

makefileContent :: String
makefileContent =
    unlines
        [ ".PHONY: all clean"
        , ""
        , "all: report.pdf"
        , ""
        , "report.pdf: report.tex"
        , "\tpdflatex report.tex"
        , "\tpdflatex report.tex  # Running pdflatex twice to resolve references"
        , "\tmake clean"
        , ""
        , "clean:"
        , "\trm -f *.aux *.log *.out *.toc"
        ]

createReport :: Double -> Double -> Double -> Double -> String
createReport truePositives falsePositives trueNegatives falseNegatives =
    unlines
        [ "\\documentclass[11pt]{article}"
        , "\\usepackage{amsmath}"
        , "\\title{Bayesian Spam Filter Project Report}"
        , "\\author{Brian Tipton}"
        , "\\date{\\today}"
        , "\\begin{document}"
        , "\\maketitle"
        , "\\section*{Performance Metrics}"
        , "\\begin{align*}"
        , "    \\text{Spam Precision} &= " ++ show spamPrecision ++ " \\\\"
        , "    \\text{Spam Recall} &= " ++ show spamRecall ++ " \\\\"
        , "    \\text{Ham Precision} &= " ++ show hamPrecision ++ " \\\\"
        , "    \\text{Ham Recall} &= " ++ show hamRecall ++ " \\\\"
        , "    \\text{Spam F-Score} &= " ++ show spamFScore ++ " \\\\"
        , "    \\text{Ham F-Score} &= " ++ show hamFScore ++ " \\\\"
        , "    \\text{Accuracy} &= " ++ show accuracy
        , "\\end{align*}"
        , "\\end{document}"
        ]
  where
    spamPrecision = Bayes.spamPrecision truePositives falsePositives
    spamRecall = Bayes.spamRecall truePositives falseNegatives
    hamPrecision = Bayes.hamPrecision trueNegatives falseNegatives
    hamRecall = Bayes.hamRecall trueNegatives falsePositives
    spamFScore = Bayes.spamFScore truePositives falsePositives falseNegatives
    hamFScore = Bayes.hamFScore trueNegatives falseNegatives falsePositives
    accuracy = Bayes.accuracy truePositives falseNegatives trueNegatives falsePositives