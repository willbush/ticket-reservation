{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Formatting         (int, sformat, stext, (%))
import           Prompts
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
  ls              <- (T.strip <$>) . T.linesCR <$> readFileUtf8 path
  finalAuditorium <- runMainLoop $ mkAuditorium ls
  printSalesReport finalAuditorium

runMainLoop :: Auditorium -> IO Auditorium
runMainLoop auditorium = do
  reserveOrExit <- promptFor1Or2 mainPrompt
  case reserveOrExit of
    1 -> do
      seats <- reserveSeats auditorium
      runMainLoop $ insertSeats auditorium seats
    _ -> do
      say "Thanks come again!\n"
      pure auditorium

reserveSeats :: Auditorium -> IO [(RowCol, Seat)]
reserveSeats auditorium = do
  say $ mapToText auditorium
  tickets <- promptForTickets
  let purchaseTotal = sum $ toTicketPrice . toTicket <$> tickets
      ticketCount = L.length tickets
      map' = auditoriumMap auditorium
      best = findBest ticketCount $ Map.toList map'
  if | null tickets -> say "No tickets purchased." >> pure []
     | null best -> say "Not enough seats available." >> pure []
     | otherwise ->
       do purchasedSeats <- promptForSeats map' tickets best
          say "Purchase complete."
          say $ "Seats purchased: " <> fmtRowCols (fst <$> purchasedSeats)
          say $ "Purchase total: " <> fmtUSD purchaseTotal
          pure purchasedSeats

promptForTickets :: IO String
promptForTickets = do
  numOfAdults   <- promptForGreaterOrEqToZero numOfAdultsPrompt
  numOfChildren <- promptForGreaterOrEqToZero numOfChildrenPrompt
  numOfSeniors  <- promptForGreaterOrEqToZero numOfSeniorsPrompt
  pure
    $  L.replicate numOfAdults 'A'
    <> L.replicate numOfChildren 'C'
    <> L.replicate numOfSeniors 'S'

promptForGreaterOrEqToZero :: Text -> IO Int
promptForGreaterOrEqToZero = promptUntilValid parse
 where
  parse s = case readMaybe s of
    Just n | n >= 0 -> Just n
    _               -> Nothing

promptForSeats
  :: Map RowCol Seat -> String -> [(RowCol, Seat)] -> IO [(RowCol, Seat)]
promptForSeats map' tickets best = do
  let formattedBest = fmtRowCols $ fst <$> best
      ticketCount   = L.length tickets
  takeBestOrSelect <- promptFor1Or2 $ seatPrompt formattedBest
  case takeBestOrSelect of
    1 -> pure $ L.zipWith
      (\t (rowCol, seat) -> (rowCol, seat { ticket = toTicket t }))
      tickets
      best
    _ -> do
      selections <- mapM (promptForSeat map' . seatSelectionPrompt)
                         [1 .. ticketCount]
      pure
        $ L.zipWith
            (\t (rowCol, seat) -> (rowCol, seat { ticket = toTicket t }))
            tickets
        $ mapMaybe
            (\rowCol ->
              Map.lookup rowCol map' >>= (\seat -> Just (rowCol, seat))
            )
            selections

promptForSeat :: Map RowCol Seat -> Text -> IO RowCol
promptForSeat map' = promptUntilValid parse
 where
  parse [rowChar, col] = case readMaybe [rowChar] of
    Just row -> do
      let rowCol = (row, upperCol)
      case Map.lookup rowCol map' of
        Just seat | isAvailable seat -> Just rowCol
        _                            -> Nothing
    _ -> Nothing
    where upperCol = C.toUpper col
  parse _ = Nothing

promptFor1Or2 :: Text -> IO Int
promptFor1Or2 = promptUntilValid parse
 where
  parse (readMaybe -> Just n) | n == 1 || n == 2 = Just n
  parse _ = Nothing

promptUntilValid :: (String -> Maybe a) -> Text -> IO a
promptUntilValid parse prompt = do
  say prompt
  line <- getLine
  case parse line of
    Just v -> pure v
    _      -> say "Invalid input." >> promptUntilValid parse prompt

printSalesReport :: Auditorium -> IO ()
printSalesReport a = do
  let map'             = auditoriumMap a
      soldTickets      = filter (/= Unreserved) $ ticket <$> Map.elems map'
      numOfAdultSold   = L.length $ filter (== Adult) soldTickets
      numOfChildSold   = L.length $ filter (== Child) soldTickets
      numOfSeniorSold  = L.length $ filter (== Senior) soldTickets
      totalTicketSales = sum $ toTicketPrice <$> soldTickets
  say $ sformat ("Total seats        | " % int) $ Map.size map'
  say $ sformat ("Total tickets sold | " % int) $ L.length soldTickets
  say $ sformat ("Adult              | " % int) numOfAdultSold
  say $ sformat ("Child              | " % int) numOfChildSold
  say $ sformat ("Senior             | " % int) numOfSeniorSold
  say $ sformat ("Total ticket sales | " % stext) $ fmtUSD totalTicketSales
