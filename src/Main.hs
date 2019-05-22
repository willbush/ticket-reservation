{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Formatting         as F
import           RIO
import qualified RIO.Char           as C
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
  reserveOrExit <- promptFor1Or2 mainPrompt
  case reserveOrExit of
    1 -> do
      seats <- reserveSeats auditorium
      say "Purchase complete.\n"
      runMainLoop $ insertSeats auditorium seats
    _ -> say "Thanks come again!"

reserveSeats :: Auditorium -> IO [(RowCol, Seat)]
reserveSeats auditorium = do
  say $ mapToText auditorium
  tickets <- promptForTickets
  let ticketCount = L.length tickets
      map' = auditoriumMap auditorium
      best = findBest ticketCount $ Map.toList map'
  takeBestOrSelect <- promptFor1Or2 $ seatPrompt best
  case takeBestOrSelect of
    1 ->
      pure $
      L.zipWith
        (\t (rowCol, seat) -> (rowCol, seat {ticket = toTicket t}))
        tickets
        best
    _ -> do
      selections <-
        mapM
          (promptUntilValid' parseRowCol (`Map.member` map') . seatSelectionPrompt)
          [1 .. ticketCount]
      pure $
        L.zipWith
          (\t (rowCol, seat) -> (rowCol, seat {ticket = toTicket t}))
          tickets $
          mapMaybe
            (\rowCol -> Map.lookup rowCol map' >>= (\seat -> Just (rowCol, seat)))
            selections

parseRowCol :: String -> RowCol
parseRowCol [rowChar, col] =
  case readMaybe [rowChar] of
    Just row -> (row, C.toUpper col)
    Nothing  -> (0, '?')
parseRowCol _ = (0, '?')

promptFor1Or2 :: Text -> IO Int
promptFor1Or2 prompt =
  promptUntilValid (\n -> n == 1 || n == 2) prompt :: IO Int

promptUntilValid :: (Read a) => (a -> Bool) -> Text -> IO a
promptUntilValid = promptUntilValid' id

promptUntilValid' :: (Read a) => (a -> b) -> (b -> Bool) -> Text -> IO b
promptUntilValid' parse isValid prompt = do
  say prompt
  line <- getLine
  case parse <$> readMaybe line of
    Just v | isValid v -> pure v
    _ -> do
      say "Invalid input."
      promptUntilValid' parse isValid prompt

promptForTickets :: IO String
promptForTickets = do
  numOfAdults   <- promptUntilValid (>= 0) numOfAdultsPrompt   :: IO Int
  numOfChildren <- promptUntilValid (>= 0) numOfChildrenPrompt :: IO Int
  numOfSeniors  <- promptUntilValid (>= 0) numOfSeniorsPrompt  :: IO Int
  pure $
    L.replicate numOfAdults   'A' <>
    L.replicate numOfChildren 'C' <>
    L.replicate numOfSeniors  'S'

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
