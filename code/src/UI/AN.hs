{- |
Module      : UI.AN
Description :
-}
module UI.AN
  ( pieceToAN
  , pieceToFEN
  , squareToAN
  , moveToAN
  ) where

import Data.Castle
import Data.Bitboard
import Data.Board
import Data.Colour
import Data.Move
import Data.Piece
import Data.Square

import Data.Char
import Data.Maybe

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

moveToAN :: Board -> Move -> String
moveToAN b mv =
  let
    sq  = _origin mv
    sq' = _target mv
    mp  = fmap snd $ b !? sq
    mp' = fmap snd $ _captured mv
    mf  = _moveFlag mv
  in
    f sq mp ++ mid mp' ++ f sq' mp' ++ g mf
  where
    mid mp = maybe " - " (\_ -> " x ") mp
    f sq (Just P) = " " ++ squareToAN sq
    f sq (Just p) = pieceToAN p ++ squareToAN sq
    f sq Nothing  = " " ++ squareToAN sq
    g mf = " | " ++ show mf
