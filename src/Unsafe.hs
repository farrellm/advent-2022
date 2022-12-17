{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Unsafe
  ( theValue,
  )
where

import Data.Set qualified as S

theValue :: Set a -> a
theValue s =
  let [x] = S.toList s
   in x
