{- |
Module      : Data.Colour
Description :
-}
module Data.Colour
  ( Colour
    ( White
    , Black
    )
  , opposite
  ) where

import Data.Ix

-- | Colour
data Colour
  = White
  | Black
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

opposite :: Colour -> Colour
opposite White = Black
opposite Black = White
