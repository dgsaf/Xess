{- |
Module      : UI.AN
Description :
-}
module UI.An
  ( pieceToAN
  , pieceToFEN
  , squareToAN
  ) where

import Data.Colour
import Data.Piece
import Data.Square

import Data.Char

pieceToAN :: Piece -> String
pieceToAN P = "P"
pieceToAN N = "N"
pieceToAN B = "B"
pieceToAN R = "R"
pieceToAN Q = "Q"
pieceToAN K = "K"

pieceToFEN :: (Colour, Piece) -> String
pieceToFEN (White, p) = fmap toUpper $ pieceToAN $ p
pieceToFEN (Black, p) = fmap toLower $ pieceToAN $ p

squareToAN :: Square -> String
squareToAN sq = [chr $ 97 + coordX sq, chr $ 49 + coordY sq]
