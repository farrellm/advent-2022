module Day4 (part1, part2) where

import Advent
import Text.Megaparsec
import Text.Megaparsec.Char

row :: Parser (Int, Int, Int, Int)
row = (,,,) <$> decimal <*> ("-" *> decimal) <*> ("," *> decimal) <*> ("-" *> decimal)

fullyContains :: (Int, Int, Int, Int) -> Bool
fullyContains (a, b, c, d) = (a <= c && b >= d) || (a >= c && b <= d)

countTrue :: [Bool] -> Int
countTrue = sum . fmap fromEnum

part1 :: IO ()
part1 = do
  ps <- unsafeParse (row `sepEndBy` eol) "data/input4.txt"
  print (countTrue $ fullyContains <$> ps)

overlaps :: (Int, Int, Int, Int) -> Bool
overlaps x@(a, b, c, d) = (a <= c && b >= c) || (a <= d && b >= d) || fullyContains x

part2 :: IO ()
part2 = do
  ps <- unsafeParse (row `sepEndBy` eol) "data/input4.txt"
  print (countTrue $ overlaps <$> ps)
