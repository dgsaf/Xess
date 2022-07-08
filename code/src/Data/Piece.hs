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
  , isSlideB, isSlideR
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

isSlideB :: Piece -> Bool
isSlideB B = True
isSlideB Q = True
isSlideB _ = False

isSlideR :: Piece -> Bool
isSlideR R = True
isSlideR Q = True
isSlideR _ = False
