{- |
Module      : Data.Castle
Description :
-}
module Data.Castle
  ( Side
    ( Short
    , Long
    )
  , initialSquareK
  , castleSquaresK, castleSquaresR
  , maybeCastleSquaresK

  , Castling
  , defaultCastling
  , enable, disable
  , movedK, movedR
  , canCastle
  ) where

import Data.Colour
import Data.Square

import Data.Ix

-- | Castling Side
data Side
  = Short
  | Long
  deriving (Eq, Ord, Ix, Enum, Bounded, Read, Show)

-- | Castling Squares
initialSquareK :: Colour -> Square
initialSquareK White = toEnum 4
initialSquareK Black = toEnum 60

initialSquareR :: (Colour, Side) -> Square
initialSquareR (White, Short) = toEnum 7
initialSquareR (White, Long)  = toEnum 0
initialSquareR (Black, Short) = toEnum 63
initialSquareR (Black, Long)  = toEnum 56

finalSquareK :: (Colour, Side) -> Square
finalSquareK (White, Short) = toEnum 6
finalSquareK (White, Long)  = toEnum 2
finalSquareK (Black, Short) = toEnum 62
finalSquareK (Black, Long)  = toEnum 58

finalSquareR :: (Colour, Side) -> Square
finalSquareR (White, Short) = toEnum 5
finalSquareR (White, Long)  = toEnum 3
finalSquareR (Black, Short) = toEnum 61
finalSquareR (Black, Long)  = toEnum 59

castleSquaresK :: (Colour, Side) -> (Square, Square)
castleSquaresK (c, s) = (initialSquareK c, finalSquareK (c, s))

castleSquaresR :: (Colour, Side) -> (Square, Square)
castleSquaresR (c, s) = (initialSquareR (c, s), finalSquareR (c, s))

maybeCastleSquaresK :: (Square, Square) -> Maybe (Colour, Side)
maybeCastleSquaresK (sq, sq')
  | (sq, sq') == castleSquaresK (White, Short) = Just (White, Short)
  | (sq, sq') == castleSquaresK (White, Long)  = Just (White, Long)
  | (sq, sq') == castleSquaresK (Black, Short) = Just (Black, Short)
  | (sq, sq') == castleSquaresK (Black, Long)  = Just (Black, Long)
  | otherwise                                  = Nothing

-- | Castling Rights
data Castling
  = Castling
  { _whiteS :: Bool
  , _whiteL :: Bool
  , _blackS :: Bool
  , _blackL :: Bool
  } deriving (Eq, Ord, Ix, Read, Show)

-- | Constructor
defaultCastling :: Castling
defaultCastling =
  Castling
  { _whiteS = True
  , _whiteL = True
  , _blackS = True
  , _blackL = True
  }

-- | Incremental Update
enable :: (Colour, Side) -> Castling -> Castling
enable (White, Short) ct = ct {_whiteS = True}
enable (White, Long)  ct = ct {_whiteL = True}
enable (Black, Short) ct = ct {_blackS = True}
enable (Black, Long)  ct = ct {_blackL = True}

disable :: (Colour, Side) -> Castling -> Castling
disable (White, Short) ct = ct {_whiteS = False}
disable (White, Long)  ct = ct {_whiteL = False}
disable (Black, Short) ct = ct {_blackS = False}
disable (Black, Long)  ct = ct {_blackL = False}

movedK :: Colour -> Castling -> Castling
movedK c ct = disable (c, Short) . disable (c, Long) $ ct

movedR :: (Colour, Side) -> Castling -> Castling
movedR (c, s) ct = disable (c, s) $ ct

-- | Query
canCastle :: Castling -> (Colour, Side) -> Bool
canCastle ct (White, Short) = _whiteS ct
canCastle ct (White, Long)  = _whiteL ct
canCastle ct (Black, Short) = _blackS ct
canCastle ct (Black, Long)  = _blackL ct
