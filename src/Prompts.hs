{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Prompts where

import           Formatting   (int, sformat, (%))
import           RIO
import           TicketSystem

mainPrompt :: Text
mainPrompt = "\n1. Reserve Seats\n2. Exit\n"

seatPrompt :: Text -> Text
seatPrompt formattedBest = mconcat
  [ "1. Take seats closet to the center: "
  , formattedBest
  , "\n"
  , "2. Select your seats\n"
  ]

numOfAdultsPrompt :: Text
numOfAdultsPrompt = "How many adult tickets (" <> fmtUSD adultPrice <> "):"

numOfChildrenPrompt :: Text
numOfChildrenPrompt = "How many child tickets (" <> fmtUSD childPrice <> "):"

numOfSeniorsPrompt :: Text
numOfSeniorsPrompt = "How many senior tickets (" <> fmtUSD seniorPrice <> "):"

seatSelectionPrompt :: Int -> Text
seatSelectionPrompt = sformat
  ("Please enter your seat selection (e.g. 1A) for ticket # " % int % ".")
