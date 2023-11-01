module Util.Tex where

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

texImports =
    unlines
        [ ""
        ]