module Advent
  ( module X,
    Parser,
    Dir4 (..),
    Dir8 (..),
    rotLeft,
    rotRight,
    unsafeParse,
    parseText,
    readInt,
    md5,
    mv4,
    mv8,
    juxt,
    (#!),
    (#!!),
    (#!!!),
    (#!?),
    (#!!?),
    (#!!!?),
    printGrid,
    symbol,
    parens,
    intersections,
    frequencies,
    mode,
  )
where

import Control.Monad.Extra as X (loop, loopM)
import Crypto.Hash (MD5, hash)
import Data.List as X (maximum, minimum)
import Data.List qualified as L
import Data.List.Extra as X (chunksOf)
import Data.Map as X ((!))
import Data.Map qualified as M
import Data.Set as X (intersection, union, (\\))
import Data.Vector as X (Vector)
import Data.Vector qualified as V
import Optics as X hiding (uncons)
import Relude.Extra.Enum as X
import Relude.Extra.Foldable1 as X
import Relude.Extra.Group as X
import Relude.Extra.Map as X
import Relude.Extra.Tuple as X
import Relude.Unsafe as X (read, (!!))
import Text.Megaparsec (Parsec, errorBundlePretty)
import Text.Megaparsec qualified as Mp
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as X (binary, decimal, hexadecimal, octal, signed)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude as X hiding (many, some)

type Parser = Parsec Void Text

data Dir4 = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

data Dir8 = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Show, Eq, Ord, Enum, Bounded)

data V2 a = V2 {x2 :: !a, y2 :: !a}
  deriving (Generic, Show, Eq, Ord, Functor)

data V3 a = V3 {x3 :: !a, y3 :: !a, z3 :: !a}
  deriving (Generic, Show, Eq, Ord, Functor)

instance Field1 (V2 a) (V2 a) a a where
  _1 = #x2

instance Field2 (V2 a) (V2 a) a a where
  _2 = #y2

instance Field1 (V3 a) (V3 a) a a where
  _1 = #x3

instance Field2 (V3 a) (V3 a) a a where
  _2 = #y3

instance Field3 (V3 a) (V3 a) a a where
  _3 = #z3

rotLeft :: Dir4 -> Dir4
rotLeft N = W
rotLeft W = S
rotLeft S = E
rotLeft E = N

rotRight :: Dir4 -> Dir4
rotRight N = E
rotRight E = S
rotRight S = W
rotRight W = N

readInt :: (ToString s) => s -> Int
readInt = read . toString

unsafeParse :: (MonadIO m) => Parser a -> FilePath -> m a
unsafeParse p f = do
  mes <- Mp.parse p f . decodeUtf8 <$> readFileBS f
  case mes of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right es -> pure es

parseText :: (MonadIO m) => Parser a -> Text -> m a
parseText p t = do
  let mes = Mp.parse p "" t
  case mes of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right es -> pure es

md5 :: Text -> Text
md5 = show . hash @ByteString @MD5 . encodeUtf8

mv4 :: (Eq a, Bounded a, Enum a) => V2 a -> Dir4 -> V2 a
mv4 (V2 r c) N = V2 (prev r) c
mv4 (V2 r c) S = V2 (next r) c
mv4 (V2 r c) E = V2 r (next c)
mv4 (V2 r c) W = V2 r (prev c)

mv8 :: (Eq a, Bounded a, Enum a) => V2 a -> Dir8 -> V2 a
mv8 (V2 r c) NN = V2 (prev r) c
mv8 (V2 r c) SS = V2 (next r) c
mv8 (V2 r c) EE = V2 r (next c)
mv8 (V2 r c) WW = V2 r (prev c)
mv8 (V2 r c) NE = V2 (prev r) (next c)
mv8 (V2 r c) SE = V2 (next r) (next c)
mv8 (V2 r c) NW = V2 (prev r) (prev c)
mv8 (V2 r c) SW = V2 (next r) (prev c)

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g x = (f x, g x)

(#!) :: Vector a -> Int -> a
(#!) = (V.!)

(#!!) :: Vector (Vector a) -> (Int, Int) -> a
v #!! (a, b) = v V.! a V.! b

(#!!!) :: Vector (Vector (Vector a)) -> (Int, Int, Int) -> a
v #!!! (a, b, c) = v V.! a V.! b V.! c

(#!?) :: Vector a -> Int -> Maybe a
(#!?) = (V.!?)

(#!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
v #!!? (a, b) = v V.!? a >>= (V.!? b)

(#!!!?) :: Vector (Vector (Vector a)) -> (Int, Int, Int) -> Maybe a
v #!!!? (a, b, c) = v V.!? a >>= (V.!? b) >>= (V.!? c)

printGrid :: Map (V2 Int) Char -> IO ()
printGrid m = do
  let minR = minimum $ (^. _1) <$> keys m
      maxR = maximum $ (^. _1) <$> keys m
      minC = minimum $ (^. _2) <$> keys m
      maxC = minimum $ (^. _2) <$> keys m
  for_ [minR .. maxR] $ \r -> do
    for_ [minC .. maxC] $ \c ->
      case m !? V2 r c of
        Nothing -> putStr "."
        Just x -> putStr [x]
    putStrLn ""

sc :: Parser ()
sc = void $ Mp.many (char ' ')

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

intersections :: (Ord a) => [Set a] -> Set a
intersections = L.foldl1' X.intersection

frequencies :: (Ord a, Foldable f) => f a -> Map a Int
frequencies = flipfoldl' (M.alter (Just . maybe 1 next)) M.empty

mode :: (Ord a, Foldable f) => f a -> [a]
mode xs
  | null xs = []
  | otherwise =
      let fs = frequencies xs
          n = maximum fs
       in fst <$> filter ((== n) . snd) (M.toList fs)
