module HAD.Y2014.M04.D04.Exercise where

import Data.List(sort,group)

-- | countFigures count the different figures that composes a number
--
-- Examples:
--
-- >>> countFigures 1
-- 1
-- >>> countFigures 1000000
-- 2
-- >>> countFigures 123
-- 3
-- >>> countFigures (-12)
-- 2
-- >>> countFigures 1234567890
-- 10
-- >>> countFigures 00001
-- 1
-- >>> countFigures 0
-- 1
--

-- map head . group . sort is effectively nub
countFigures :: Integral a => a -> Int
countFigures = length . map head . group . sort . show . abs
