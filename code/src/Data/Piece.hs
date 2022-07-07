{- |
Module      : Data.Piece
Description :
-}
module Data.Piece
  ( Piece
    ( P
    , N
    , B
    , R
    , Q
    , K
    )
  ) where

import Data.Ix

-- | Piece
data Piece
  = P
  | N
  | B
  | R
  | Q
  | K
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)
