{-# LANGUAGE TypeApplications #-}

module DayX (part1, part2) where

import Advent
import Text.Megaparsec
import Text.Megaparsec.Char

pack :: Parser Int
pack = pure 0

part1 :: IO ()
part1 = do
  ps <- unsafeParse (pack `sepBy` eol) "data/test1.txt"
  print ps

part2 :: IO ()
part2 = pass
