{-# LANGUAGE TypeApplications #-}

module Day3 (part1, part2) where

import Advent
import Data.Char
import Data.Set qualified as S
import Unsafe

priority :: Char -> Int
priority c | isLower c = ord c - ord 'a' + 1
priority c = ord c - ord 'A' + 27

errorPriority :: [Char] -> Int
errorPriority cs =
  let l = length cs
      (as, bs) = splitAt (l `div` 2) cs
      c = theValue (S.fromList as `intersection` S.fromList bs)
   in priority c

part1 :: IO ()
part1 = do
  ss <- fmap toString . lines . decodeUtf8 <$> readFileBS "data/input3.txt"
  print (sum $ errorPriority <$> ss)

part2 :: IO ()
part2 = do
  ss <- fmap toString . lines . decodeUtf8 <$> readFileBS "data/input3.txt"
  let ts = theValue . intersections . fmap S.fromList <$> chunksOf 3 ss
  print (sum $ priority <$> ts)
