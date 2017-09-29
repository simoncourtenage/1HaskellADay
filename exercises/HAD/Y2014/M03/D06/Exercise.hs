module HAD.Y2014.M03.D06.Exercise where

-- | takeStrictlyLessThan take elements of a list whils their sum is
-- _strictly_ less than a given number
--
-- Point-free: I didnt' try without parameter, you can easily "hide" the 2nd
-- parameter (ie. takeStrictlyLessThan x = …)
-- Level: MEDIUM
--
-- Examples:
-- >>> takeStrictlyLessThan (10::Int) [1..]
-- [1,2,3]
-- 
-- >>> takeStrictlyLessThan (3::Integer) $ repeat 1
-- [1,1]
--
-- >>> takeStrictlyLessThan (42::Int) $ []
-- []
--
-- takeStrictlyLessThan :: Choose your poison

{--
   I don't like the official solution.  It's far too complex.  I don't like my hand-coded solution,
   either.  It's far too simplistic, plus it's hand-coding the recursion.  However, I then looked at
   the performance of each, by compiling with -O2 and linking using -rtsopts, and running each using +RTS -sstderr
   using the main function

    main = do
        let result = takeStrictlyLessThan (9999999999::Int) [1..]
        let total = sum result
        putStrLn ("result is " ++ show total)

    The hand-coded solution wins when the stats are compared.  Less MUT and total time!  The
    difference is really stark if you don't use -O2.

--}

takeStrictlyLessThan :: (Num a, Ord a) => a -> [a] -> [a]
takeStrictlyLessThan a xs = f 0 a xs
    where f _ _ [] = []
          f c t (y:ys) | c + y < t = y : f (c+y) t ys
                       | otherwise = []


