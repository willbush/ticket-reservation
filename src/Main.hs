{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO
import qualified RIO.List           as L
import qualified RIO.Map            as Map
import qualified RIO.Text           as T
import           Say                (say, sayString)
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
runMainLoop a = do
  num <- promptUntilValid mainPrompt (\n -> n == 1 || n == 2) :: IO Int
  case num of
    1 -> do
      say $ mapToText a
      tickets <- promptForTickets
      let best = findBest (L.length tickets) $ Map.toList $ auditoriumMap a
      sayString $ show best
      -- TODO prompt user if they want the seats, and if not prompt for seats.
      runMainLoop a
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
numOfAdultsPrompt = "How many adult tickets:";

numOfChildrenPrompt :: Text
numOfChildrenPrompt = "How many child tickets:";

numOfSeniorsPrompt :: Text
numOfSeniorsPrompt = "How many senior tickets:";
