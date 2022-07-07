{- |
Module      : UI.Display
Description :
-}
module UI.Display
  ( display
  , displayWord
  ) where

import Data.Bitboard
import Data.Square

import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Word

display :: (Square -> a) -> (a -> String) -> [String]
display f g = fmap (concat . intersperse " " . fmap ((!) padArr)) lines
  where
    lines = [mapMaybe mkSquareXY [(x, y) | x <- [0 .. 7]] | y <- [7, 6 .. 0]]
    strArr = buildSquaresArray (g . f) :: Array Square String
    width = maximum $ fmap length strArr
    padArr = fmap (\ s -> take (width - length s) (repeat ' ') ++ s) strArr

displayWord :: Word64 -> [String]
displayWord w = display (hasSquare w) (\ b -> if b then "x" else ".")
