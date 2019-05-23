{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TicketSystem
  ( mkAuditorium
  , mapToText
  , findBest
  , toTicket
  , insertSeats
  , isAvailable
  )
where

import qualified Data.List.Extra as E
import           Data.List.Split (divvy)
import           Prelude         (pred, succ)
import           RIO
import qualified RIO.List        as L
import qualified RIO.Map         as Map
import qualified RIO.Text        as T
import           Types

-- | Makes an auditorium form a list of text where each text entry is a row of
-- seats.
mkAuditorium :: [Text] -> Auditorium
mkAuditorium [] =
  Auditorium {rowCount = 0, colCount = 0, auditoriumMap = Map.empty}
mkAuditorium rows@(row:_) =
  Auditorium
    { rowCount = rowCount'
    , colCount = colCount'
    , auditoriumMap = Map.fromList indexValuePairs
    }
  where
    rowCount' = length rows
    colCount' = T.length row
    -- | Subtract one to make it zero based (i.e. centered at the origin)
    x = fromIntegral $ colCount' - 1
    y = fromIntegral $ rowCount' - 1
    -- | mid point formula is simplified because the beginning point (for both x
    -- and y) is centered at the origin of a Cartesian plane.
    midPoint = (x / 2, y / 2)
    alphabet = ['A' .. 'Z']
    toKeyValuePair row' col ticketLetter = (key, value)
      where
        key = (row', col)
        value =
          Seat
            { ticket = toTicket ticketLetter
            , distanceFromMid = distance midPoint (toXYPoint key)
            }
    indexValuePairs =
      join $
      L.zipWith
        (\row' tickets -> L.zipWith (toKeyValuePair row') alphabet tickets)
        [1 ..] $
      fmap T.unpack rows

toTicket :: Char -> Ticket
toTicket 'A' = Adult
toTicket 'C' = Child
toTicket 'S' = Senior
toTicket _   = Unreserved

distance :: XYPoint -> XYPoint -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

-- | Both the row and columns are adjusted so that the first seat 1A is mapped
-- onto the origin Cartesian system.
toXYPoint :: (Int, Char) -> XYPoint
toXYPoint (rowNum, columnLetter) = (x, y)
 where
  x = fromIntegral $ fromEnum columnLetter - fromEnum 'A'
  y = fromIntegral rowNum - 1

-- | Takes a map and turns into a text representation that doesn't display the
-- ticket types in each seat. The following example represents a 2x2 auditorium
-- where "." represents an available seat and "#" represents a taken seat.
--
--   AB
-- 1 .#
-- 2 #.
mapToText :: Auditorium -> Text
mapToText auditorium =
  let map' = auditoriumMap auditorium
      colCount' = colCount auditorium
      groupedByRow = E.groupOn (fst . fst) $ Map.toList map'
      columnLetters = "  " <> take colCount' ['A' ..] <> "\n"
      toTicketChars = fmap (ticketToChar . ticket . snd)
      rowStrs =
        fmap
          (\row -> getRowNumStr row <> toTicketChars row <> "\n")
          groupedByRow
      gridStr = foldl' (<>) [] rowStrs
   in T.pack $ columnLetters <> gridStr

-- Truns a ticket into a character '.' to represent a unreserved seat or '#' for
-- taken seats.
ticketToChar :: Ticket -> Char
ticketToChar Unreserved = '.'
ticketToChar _          = '#'

getRowNumStr :: [(RowCol, Seat)] -> String
getRowNumStr seats =
  case L.headMaybe seats of
    Just seat -> show ((fst . fst) seat) <> " "
    Nothing   -> ""

findBest :: Int -> [(RowCol, Seat)] -> [(RowCol, Seat)]
findBest _ []    = []
findBest n seats = L.sortOn fst $
  case bestContiguous of
    [] ->
      -- | If there were no contiguous seats, then just take the best seats
      -- closest to the middle.
      takeExact n $ L.sortBy distanceFromMidThenRowCol $ filterUnreserved seats
    _ -> bestContiguous
  where
    bestContiguous =
      join
        $ fmap fst -- select just the best contiguous seats
        $ take 1 -- take the best
        -- | Sorting by the average distance then by row and column makes the
        -- first element the best. It also has the effect of deterministics
        -- distance tie breaking.
        $ L.sortBy distanceThenRowCol
        -- | Transforms the list so that each contiguous seat list is in a tuple
        -- along with the average distance between all seats in its list.
        $ fmap (\xs -> (xs, avg (map (distanceFromMid . snd) xs)))
        $ join -- join the rows
        $ fmap (filter (allAreAdj . map fst)) -- filter the rows to only contiguous
        -- | divvy up the seats in each row by how many tickets they want. Note
        -- that divvy drops any remaining elements less than n (as desired).
        $ fmap (divvy n 1)
        $ E.groupOn (fst . fst) -- group on the row
        $ L.sortOn fst -- sort on row and column
        $ filterUnreserved seats

    -- | Takes exactly n things otherwise returns an empty list
    takeExact :: Int -> [a] -> [a]
    takeExact num xs = if length result == n then result else []
      where result = take num xs

    -- | Filters down to only unreserved seats.
    filterUnreserved :: [(RowCol, Seat)] -> [(RowCol, Seat)]
    filterUnreserved = filter (\x -> ticket (snd x) == Unreserved)

    -- | returns if all the row column indices are adjacent or not.
    allAreAdj :: [RowCol] -> Bool
    allAreAdj (x : y : xs) = go x y xs
     where
      go a b []       = isAdj a b
      go a b (c : cs) = isAdj a b && go b c cs
      -- | returns if the given row column indices are adjacent or not.
      isAdj :: RowCol -> RowCol -> Bool
      isAdj (row1, col1) (row2, col2) =
        row1 == row2 && (succ col1 == col2 || pred col1 == col2)
    allAreAdj _ = False

    -- | A simplistic implementation of average.
    avg :: [Double] -> Double
    avg xs = sum' / num
      where (sum', num) = foldl' (\(a, b) x -> (a + x, b + 1)) (0, 0) xs

    -- | Helper function to sort by the distance, then the row and column.
    distanceThenRowCol
      :: ([(RowCol, Seat)], Double) -> ([(RowCol, Seat)], Double) -> Ordering
    distanceThenRowCol ([]        , d1) (_         , d2) = compare d1 d2
    distanceThenRowCol (_         , d1) ([]        , d2) = compare d1 d2
    distanceThenRowCol ((x, _) : _, d1) ((y, _) : _, d2) =
      case compare d1 d2 of
        EQ -> compare x y
        LT -> LT
        GT -> GT

    -- | Helper function to sort by the distance, then the row and column.
    distanceFromMidThenRowCol :: (RowCol, Seat) -> (RowCol, Seat) -> Ordering
    distanceFromMidThenRowCol (rc1, s1) (rc2, s2) =
      case compare (distanceFromMid s1) (distanceFromMid s2) of
        EQ -> compare rc1 rc2
        LT -> LT
        GT -> GT

insertSeats :: Auditorium -> [(RowCol, Seat)] -> Auditorium
insertSeats a seats =
  let map' = auditoriumMap a
   in a {auditoriumMap = foldl' (\m (k, v) -> Map.insert k v m) map' seats}

isAvailable :: Seat -> Bool
isAvailable (Seat Unreserved _ ) = True
isAvailable _                    = False
