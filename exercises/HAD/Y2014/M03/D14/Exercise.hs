module HAD.Y2014.M03.D14.Exercise where

-- $setup
-- >>> import Control.Applicative
-- >>> let s2 = ([id,(+1)] <*>) . pure

-- | groupByStraights Group elements in a list by "straights"
-- i.e: consecutive elements are grouped together.
-- 
-- Examples:
--
-- >>> groupByStraights [1,2,5,6,8]
-- [[1,2],[5,6],[8]]
--
-- >>> take 3 . groupByStraights $ [0..] >>= s2
-- [[0,1],[1,2],[2,3]]
--
-- >>> take 4 . groupByStraights $ "abbccddeeeeeeeeeee"
-- ["ab","bc","cd","de"]
--
-- groupByStraights :: START HERE

{--
  This is another example where the hand-coded example outperforms the official solution.  Have a look in the
  Main.hs file.
--}

groupByStraights :: (Enum a, Eq a) => [a] -> [[a]]
groupByStraights [] = []
groupByStraights [x] = [[x]]
groupByStraights (x:y:xs) | y == succ x = [x,y] : groupByStraights xs
                          | otherwise   = [x] : groupByStraights (y:xs)
