{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

module Types where

import           RIO

data Ticket = Adult | Child | Senior | Unreserved
  deriving (Show, Eq)

data Seat = Seat
  { ticket          :: Ticket
  , distanceFromMid :: Double
  }
  deriving (Show, Eq)

type XYPoint = (Double, Double)

type RowCol = (Int, Char)

data Auditorium = Auditorium
  { rowCount      :: Int
  , colCount      :: Int
  , auditoriumMap :: Map RowCol Seat
  }
