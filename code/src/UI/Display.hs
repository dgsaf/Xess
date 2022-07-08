{- |
Module      : UI.Display
Description :
-}
module UI.Display
  ( display
  , displayWord
  , displayWordsH, displayWordsV, displayWordsHV
  , displayBoard, displayBoardPieces
  ) where

import Data.Bitboard
import Data.Board
import Data.Colour
import Data.Piece
import Data.Rotated
import Data.Square
import UI.AN

import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Word

-- |
ranks :: [[Square]]
ranks = [mapMaybe mkSquareXY [(x, y) | x <- [0 .. 7]] | y <- [7, 6 .. 0]]

display :: (Square -> a) -> (a -> String) -> [String]
display f g = fmap (concat . intersperse " " . fmap ((!) padArr)) ranks
  where
    strArr = buildSquaresArray (g . f) :: Array Square String
    width = maximum $ fmap length strArr
    padArr = fmap (\ s -> take (width - length s) (repeat ' ') ++ s) strArr

-- |
joinH :: [[String]] -> [String]
joinH grids =
  transpose . concat . fmap transpose . intersperse (take 8 $ repeat "  ")
  $ grids

joinV :: [[String]] -> [String]
joinV grids = concat . intersperse [""] $ grids

-- |
displayWord :: Word64 -> [String]
displayWord w = display (hasSquare w) (\ b -> if b then "x" else ".")

displayWordsH :: [Word64] -> [String]
displayWordsH ws = joinH $ fmap displayWord ws

displayWordsV :: [Word64] -> [String]
displayWordsV ws = joinV $ fmap displayWord ws

displayWordsHV :: [[Word64]] -> [String]
displayWordsHV wss = joinV . fmap (joinH . fmap displayWord) $ wss

-- |
displayBoard :: Board -> [String]
displayBoard b = display (b !?) (maybe "." pieceToFEN)

displayBoardPieces :: Board -> [String]
displayBoardPieces b =
  displayWordsHV [[piece b (c, p) | p <- [minBound ..]] | c <- [minBound ..]]
