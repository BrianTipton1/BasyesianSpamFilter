module SpamFilter.Probability.Basyesian where

spamPrecision :: Fractional a => a -> a -> a
spamPrecision tP fP = tP / (tP + fP)

spamRecall :: Fractional a => a -> a -> a
spamRecall tP fN = tP / (tP + fN)

hamPrecision :: Fractional a => a -> a -> a
hamPrecision tN fN = tN / (tN + fN)

hamRecall :: Fractional a => a -> a -> a
hamRecall tN fP = tN / (tN + fP)

spamFScore :: Fractional a => a -> a -> a -> a
spamFScore tP fP fN =
    (2 * spamPrecision tP fP * spamRecall tP fN)
        / (spamPrecision tP fP + spamRecall tP fN)

hamFScore :: Fractional a => a -> a -> a -> a
hamFScore tN fN fP =
    (2 * hamPrecision tN fN * hamRecall tN fP)
        / (hamPrecision tN fN + hamRecall tN fP)

accuracy :: Fractional a => a -> a -> a -> a -> a
accuracy tP fN tN fP =
    (spamRecall tP fN + hamRecall tN fP) / 2