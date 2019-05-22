{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Prompts where

import           Formatting (char, fixed, int, sformat, (%))
import           RIO
import qualified RIO.Text   as T
import           Types

mainPrompt :: Text
mainPrompt = "\n1. Reserve Seats\n2. Exit\n"

seatPrompt :: [(RowCol, Seat)] -> Text
seatPrompt seats =
  mconcat
    [ "1. Take seats closet to the center: "
    , T.pack $ show $ fmap (fmtRowCol . fst) seats
    , "\n"
    , "2. Select your seats\n"
    ]

numOfAdultsPrompt :: Text
numOfAdultsPrompt =
  sformat ("How many adult tickets ($" % fixed 2 % " per ticket):") adultPrice

numOfChildrenPrompt :: Text
numOfChildrenPrompt =
  sformat ("How many child tickets ($" % fixed 2 % " per ticket):") childPrice

numOfSeniorsPrompt :: Text
numOfSeniorsPrompt =
  sformat ("How many senior tickets ($" % fixed 2 % " per ticket):") seniorPrice

seatSelectionPrompt :: Int -> Text
seatSelectionPrompt =
  sformat
    ("Please enter your seat selection (e.g. 1A) for ticket # " % int % ".")

fmtRowCol :: RowCol -> Text
fmtRowCol (row, col) = sformat (int % char) row col

adultPrice :: Double
adultPrice = 10.0

childPrice :: Double
childPrice = 5.0

seniorPrice :: Double
seniorPrice = 7.5
