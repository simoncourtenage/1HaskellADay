module HAD.Y2014.M03.D31.Exercise where

import Data.Maybe (isNothing)

-- $setup
-- >>> import Control.Applicative

-- | emptyIndices List the indices of a list of maybes that contains Nothing
--
-- prop> (all (isNothing) .) . map . (!!) <*> emptyIndices $ xs
--
emptyIndices :: [Maybe a] -> [Int]
emptyIndices = map fst . filter (isNothing . snd) . (zip [0..])
