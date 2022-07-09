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
  , isP, isN, isB, isR, isQ, isK
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

isP :: Piece -> Bool
isP P = True
isP _ = False

isN :: Piece -> Bool
isN N = True
isN _ = False

isB :: Piece -> Bool
isB B = True
isB _ = False

isR :: Piece -> Bool
isR R = True
isR _ = False

isQ :: Piece -> Bool
isQ Q = True
isQ _ = False

isK :: Piece -> Bool
isK K = True
isK _ = False

isSlideB :: Piece -> Bool
isSlideB B = True
isSlideB Q = True
isSlideB _ = False

isSlideR :: Piece -> Bool
isSlideR R = True
isSlideR Q = True
isSlideR _ = False
