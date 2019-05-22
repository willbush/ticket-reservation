{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Prompts where

import qualified Formatting as F
import           RIO
import qualified RIO.Text   as T
import           Types

mainPrompt :: Text
mainPrompt = "1. Reserve Seats\n2. Exit\n"

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
  F.sformat
    ("How many adult tickets ($" F.% F.fixed 2 F.% " per ticket):")
    adultPrice

numOfChildrenPrompt :: Text
numOfChildrenPrompt =
  F.sformat
    ("How many child tickets ($" F.% F.fixed 2 F.% " per ticket):")
    childPrice

numOfSeniorsPrompt :: Text
numOfSeniorsPrompt =
  F.sformat
    ("How many senior tickets ($" F.% F.fixed 2 F.% " per ticket):")
    seniorPrice

seatSelectionPrompt :: Int -> Text
seatSelectionPrompt =
  F.sformat
    ("Please enter your seat selection (e.g. 1A) for ticket # " F.% F.int F.%
     ".")

fmtRowCol :: RowCol -> Text
fmtRowCol (row, col) = F.sformat (F.int F.% F.char) row col

adultPrice :: Double
adultPrice = 10.0;

childPrice :: Double
childPrice = 5.0;

seniorPrice :: Double
seniorPrice = 7.5;
