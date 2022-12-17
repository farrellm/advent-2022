module Day2 (part1, part2) where

import Advent hiding (S, W)
import Text.Megaparsec
import Text.Megaparsec.Char

data RPS = R | P | S
  deriving (Show, Eq, Ord)

data WTL = W | T | L
  deriving (Show, Eq, Ord)

rps :: Parser RPS
rps = (("A" <|> "X") $> R) <|> (("B" <|> "Y") $> P) <|> (("C" <|> "Z") $> S)

wtl :: Parser WTL
wtl = ("Z" $> W) <|> ("Y" $> T) <|> ("X" $> L)

game :: Parser (RPS, RPS)
game = (,) <$> rps <*> (" " *> rps)

game2 :: Parser (RPS, WTL)
game2 = (,) <$> rps <*> (" " *> wtl)

rpsScore :: RPS -> Int
rpsScore R = 1
rpsScore P = 2
rpsScore S = 3

resultScore :: RPS -> RPS -> Int
resultScore R P = 6
resultScore P S = 6
resultScore S R = 6
resultScore a b | a == b = 3
resultScore _ _ = 0

score :: RPS -> RPS -> Int
score a b = resultScore a b + rpsScore b

calcPlay :: RPS -> WTL -> RPS
calcPlay R W = P
calcPlay P W = S
calcPlay S W = R
calcPlay x T = x
calcPlay R L = S
calcPlay S L = P
calcPlay P L = R

part1 :: IO ()
part1 = do
  ps <- unsafeParse (game `sepEndBy` eol) "data/input2.txt"
  print (sum $ uncurry score <$> ps)

part2 :: IO ()
part2 = do
  ps <- unsafeParse (game2 `sepEndBy` eol) "data/input2.txt"
  let ms = uncurry calcPlay <$> ps
  print (sum $ zipWith score (fst <$> ps) ms)
