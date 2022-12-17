module Day1 (part1, part2) where

import Advent
import Text.Megaparsec
import Text.Megaparsec.Char

pack :: Parser [Int]
pack = decimal `sepEndBy` eol

part1 :: IO ()
part1 = do
  ps <- unsafeParse (pack `sepBy` eol) "data/input1.txt"
  print (maximum $ sum <$> ps)

part2 :: IO ()
part2 = do
  ps <- unsafeParse (pack `sepBy` eol) "data/input1.txt"
  print (sum . take 3 . reverse . sort $ sum <$> ps)
