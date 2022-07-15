{- |
Module      : UI.Display
Description :
-}
module UI.Display
  ( display
  , displaysH, displaysV, displaysHV

  , displayWord, displayWordsSquares

  , displayBoard, displayBoardPieces
  ) where

import Data.Bitboard
import Data.Board
import Data.BoardState
import Data.Castle
import Data.Colour
import Data.Move
import Data.Pawn
import Data.Piece
import Data.Rotated
import Data.Square
import UI.AN

import Data.Array.IArray
import Data.Bits
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

displaysH :: (a -> [String]) -> [a] -> [String]
displaysH f as = joinH . fmap f $ as

displaysV :: (a -> [String]) -> [a] -> [String]
displaysV f as = joinV . fmap f $ as

displaysHV :: (a -> [String]) -> [[a]] -> [String]
displaysHV f ass = joinV . fmap (joinH . fmap f) $ ass

-- |
displayWord :: Word64 -> [String]
displayWord w = display (hasSquare w) (\ b -> if b then "x" else ".")

displayWordsSquares :: (Square -> Word64) -> [String]
displayWordsSquares f = displaysHV displayWord $ fmap (fmap f) ranks

-- |
displayBoard :: Board -> [String]
displayBoard b = display (b !?) (maybe "." pieceToFEN)

displayBoardPieces :: Board -> [String]
displayBoardPieces b = displaysHV displayWord cpwss
  where
    cpwss = [[piece b (c, p) | p <- [minBound ..]] | c <- [minBound ..]]
