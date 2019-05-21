{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Formatting         as F
import           RIO
import qualified RIO.List           as L
import qualified RIO.Map            as Map
import qualified RIO.Text           as T
import           Say                (say)
import           System.Environment (getArgs)
import           System.IO          (getLine)
import           TicketSystem
import           Types

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "./test-cases/A1.txt" (L.headMaybe args)
  ls <- (T.strip <$>) . T.linesCR <$> readFileUtf8 path
  runMainLoop $ mkAuditorium ls

runMainLoop :: Auditorium -> IO ()
runMainLoop auditorium = do
  let promptFor1Or2 prompt =
        promptUntilValid prompt (\n -> n == 1 || n == 2) :: IO Int
  num <- promptFor1Or2 mainPrompt
  case num of
    1 -> do
      say $ mapToText auditorium
      tickets <- promptForTickets
      let map' = auditoriumMap auditorium
          best = findBest (L.length tickets) $ Map.toList map'
          prompt =
            mconcat
              [ "1. Take seats closet to the center: "
              , T.pack $ show $ fmap (fmtRowCol . fst) best
              , "\n"
              , "2. Select your seats\n"
              ]
      response <- promptFor1Or2 prompt
      case response of
        1 -> do
          -- Reserve the best seats.
          let bestReserved =
                L.zipWith
                  (\t (rowCol, seat) -> (rowCol, seat {ticket = toTicket t}))
                  tickets
                  best
          runMainLoop $
            auditorium
              { auditoriumMap =
                  foldl' (\m (k, v) -> Map.insert k v m) map' bestReserved
              }
        _ -> do
          say "todo"
          runMainLoop auditorium
    _ -> say "Thanks come again!"

promptUntilValid :: (Read a) => Text -> (a -> Bool) -> IO a
promptUntilValid prompt isValid = do
  say prompt
  line <- getLine
  case readMaybe line of
    Just value | isValid value -> pure value
    _ -> do
      say "Invalid input."
      promptUntilValid prompt isValid

promptForTickets :: IO String
promptForTickets = do
  numOfAdults <- promptUntilValid numOfAdultsPrompt (>= 0) :: IO Int
  numOfChildren <- promptUntilValid numOfChildrenPrompt (>= 0) :: IO Int
  numOfSeniors <- promptUntilValid numOfSeniorsPrompt (>= 0) :: IO Int
  pure $
    L.replicate numOfAdults 'A'
    <> L.replicate numOfChildren 'C'
    <> L.replicate numOfSeniors 'S'

mainPrompt :: Text
mainPrompt = "1. Reserve Seats\n2. Exit\n"

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

fmtRowCol :: RowCol -> Text
fmtRowCol (row, col) = F.sformat (F.int F.% F.char) row col

adultPrice :: Double
adultPrice = 10.0;

childPrice :: Double
childPrice = 5.0;

seniorPrice :: Double
seniorPrice = 7.5;
