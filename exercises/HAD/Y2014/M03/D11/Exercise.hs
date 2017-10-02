module HAD.Y2014.M03.D11.Exercise where

import Data.Char(ord,chr)
import Data.List(find)
import Data.Maybe(maybe)

-- | lcAlphabetFrom
-- Display the alaphabet in lower cas, starting from the letter given in
-- parameter.
-- If the parameter is not a lowercase letter, displays the alphabet from 'a'
--
-- Point-free is quite easy
--
-- Examples:
--
-- >>> lcAlphabetFrom 'a'
-- "abcdefghijklmnopqrstuvwxyz"
--
-- >>> lcAlphabetFrom 'e'
-- "efghijklmnopqrstuvwxyzabcd"
--
-- >>> lcAlphabetFrom '`'
-- "abcdefghijklmnopqrstuvwxyz"
--
-- >>> lcAlphabetFrom '{'
-- "abcdefghijklmnopqrstuvwxyz"

{--
  My solution is much more heavyweight than it needs to be, compared with the official solution.  It shows
  that the Prelude is something really worth knowing well.
--}

lcAlphabetFrom :: Char -> String
lcAlphabetFrom = maybe as (\a -> take 26 . drop (ord a - ord 'a') $ cycle as). (flip find) as . (==)
    where as = ['a'..'z']
