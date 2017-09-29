module HAD.Y2014.M03.D04.Exercise where

-- | checkSort verify if a given list is sorted
-- Point-free version is hard to find BUT is readable.
-- Level: Medium
--
-- Examples:
--
-- >>> checkSort ([1..10] :: [Int])
-- True
--
-- >>> checkSort $ ([1,3,2] :: [Int])
-- False
--
-- >>> checkSort []
-- True
--

-- This is my humble solution.  A bit too humble, actually, after reading
-- the official solution.
checkSort :: Ord a => [a] -> Bool
checkSort [] = True
checkSort [x] = True
checkSort (a:b:cs) | a <= b    = checkSort (b:cs)
                   | otherwise = False


{-- This is one of the official solutions, which I would like to dissect to understand
    how it works.

    Firstly, the key part is the expression (zipWith (<=) <*> tail).  The type of this
    is Ord a => [a] -> [Bool].  Running it with an example gives
    (zipWith (<=) <*> tail) [1,2,3,4] =====> [True,True,True]
    (zipWith (<=) <*> tail) [9,1,2,3,4] =====> [False,True,True,True]

    How does it work?  The type of <*> is 
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    The expression 'zipWith (<=)'' is the first argument to <*>, while tail is the second, so
    the types align like this

    <*> :: f (a -> b) -> f a -> f b
    zipWith (<=) :: [a] -> [a] -> [Bool]
    tail :: [a] -> [a]

    So the 'f' in the type of <*> is '(->) [a]' - in other words, we are dealing with the
    applicative of functions.  The type of <*> can therefore be specialised as
    <*> :: ((->) [a]) ([a] -> [Bool]) -> ((->) [a]) [a] -> ((->) [a]) [Bool]

    Remember that <*> when applied to function context, acts like parallel application - in other
    words, each side of <*> are applied to a single argument in parallel.  This single argument is
    the list to be checked. And what happens is that tail drops the first element and 'zipWith (<=)'
    takes the original list as its first list argument, and the result of tail as its second.  So we
    zip the two lists together using (<=) to create a list of Bools.

    So if the original list is [1,3,2,5], we zipWith

        [1,3,2,5] and
        [3,2,5]

    and end up with [1 <= 3, 3 <= 2, 2 <= 5] = [True,False,True].

    Then we summarise this list using 'and'.

    This is a really nice exercise in using the applicative of functions to create a much more elegant
    solution than my humble effort!

--}
checkSort' :: Ord a => [a] -> Bool
checkSort' =  and . (zipWith (<=) <*> tail)
