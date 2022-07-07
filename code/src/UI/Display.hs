{- |
Module      : UI.Display
Description :
-}
module UI.Display
  ( display
  , displayWord
  ) where

import Data.Bitboard
import Data.Rotated
import Data.Square

import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Word

ranks :: [[Square]]
ranks = [mapMaybe mkSquareXY [(x, y) | x <- [0 .. 7]] | y <- [7, 6 .. 0]]

display :: (Square -> a) -> (a -> String) -> [String]
display f g = fmap (concat . intersperse " " . fmap ((!) padArr)) ranks
  where
    strArr = buildSquaresArray (g . f) :: Array Square String
    width = maximum $ fmap length strArr
    padArr = fmap (\ s -> take (width - length s) (repeat ' ') ++ s) strArr

displayWord :: Word64 -> [String]
displayWord w = display (hasSquare w) (\ b -> if b then "x" else ".")

displayWords :: [Word64] -> [String]
displayWords ws = transpose . concat . fmap transpose
                  . intersperse (take 8 $ repeat "  ")
                  $ fmap displayWord ws
