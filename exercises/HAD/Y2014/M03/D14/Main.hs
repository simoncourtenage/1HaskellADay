module Main where

import Data.List

{--
  This code is used to test the performance of both solutions.  Compiled with -O2 and linked
  as
    ghc -o main Main.o -rtsopts
  Run using
    ./main +RTS -sstderr

  My solution - example run
  -------------------------
  length of list is 10000000
   1,440,118,544 bytes allocated in the heap
         418,504 bytes copied during GC
          77,664 bytes maximum residency (2 sample(s))
          22,040 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2789 colls,     0 par    0.005s   0.006s     0.0000s    0.0005s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0004s

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time    0.280s  (  1.620s elapsed)
  GC      time    0.005s  (  0.007s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.287s  (  1.629s elapsed)

  %GC     time       1.7%  (0.4% elapsed)

  Alloc rate    5,137,555,809 bytes per MUT second

  Productivity  98.2% of total user, 17.3% of total elapsed

  Official solution - example run
  -------------------------------
  length of list is 10000000
   5,360,118,520 bytes allocated in the heap
       2,231,712 bytes copied during GC
          77,880 bytes maximum residency (2 sample(s))
          21,152 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10415 colls,     0 par    0.017s   0.022s     0.0000s    0.0004s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0004s

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time    0.931s  (  1.909s elapsed)
  GC      time    0.017s  (  0.023s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.949s  (  1.934s elapsed)

  %GC     time       1.8%  (1.2% elapsed)

  Alloc rate    5,759,784,184 bytes per MUT second

  Productivity  98.2% of total user, 48.2% of total elapsed

  Notes
  -----
  - difference in size of heap
  - difference in bytes copied in GC
  - difference in MUT (actual program execution time)
  - difference in GC time

--}
groupByStraights :: (Enum a, Eq a) => [a] -> [[a]]
groupByStraights [] = []
groupByStraights [x] = [[x]]
groupByStraights (x:y:xs) | y == succ x = [x,y] : groupByStraights xs
                          | otherwise   = [x] : groupByStraights (y:xs)

groupByStraights' :: (Enum a, Eq a) => [a] -> [[a]]
groupByStraights' =
    map (map fst)
    . groupBy (const snd)
    . (zip <*> (zipWith ((==) . pred) <*> (toEnum 0:)))

main = do
    putStrLn "Which do you want to run? Mine (1) or Official (2): "
    resp <- getLine
    let opt = read resp :: Int
    let rs = take 10000000 $ repeat 1
    let ans = case opt of 
                1 -> groupByStraights rs
                2 -> groupByStraights' rs
    let len = length ans
    putStrLn ("length of list is " ++ show len)
 