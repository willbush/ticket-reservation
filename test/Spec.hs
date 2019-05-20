{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec where

import           RIO
import qualified RIO.Map         as Map
import           Test.Hspec
import           Test.QuickCheck as Q
import           TicketSystem
import           Types

main :: IO ()
main = hspec $ do
  describe "find best basic tests" $ do
   it "can find the single best seat" $
     let
       auditorium = mkAuditorium ["AAA" , "C.S" , "AAC"]
       mapList = Map.toList $ auditoriumMap auditorium
       bestSeat = ((2, 'B'), Seat {ticket = Unreserved, distanceFromMid = 0.0})
       seatList =
         [ ((1, 'A'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((1, 'B'), Seat {ticket = Adult, distanceFromMid = 1.0})
         , ((1, 'C'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((2, 'A'), Seat {ticket = Child, distanceFromMid = 1.0})
         , bestSeat
         , ((2, 'C'), Seat {ticket = Senior, distanceFromMid = 1.0})
         , ((3, 'A'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((3, 'B'), Seat {ticket = Adult, distanceFromMid = 1.0})
         , ((3, 'C'), Seat {ticket = Child, distanceFromMid = 1.4142135623730951})
         ]
      in do
       rowCount auditorium `shouldBe` 3
       colCount auditorium `shouldBe` 3
       mapList `shouldBe` seatList
       findBest 1 seatList `shouldBe` [bestSeat]

   it "can find the best 2 middle seats" $
     let
       auditorium = mkAuditorium ["AAA" , "C.." , "AAC"]
       mapList = Map.toList $ auditoriumMap auditorium
       fstBest = ((2, 'B'), Seat {ticket = Unreserved, distanceFromMid = 0.0})
       sndBest = ((2, 'C'), Seat {ticket = Unreserved, distanceFromMid = 1.0})
       seatList =
         [ ((1, 'A'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((1, 'B'), Seat {ticket = Adult, distanceFromMid = 1.0})
         , ((1, 'C'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((2, 'A'), Seat {ticket = Child, distanceFromMid = 1.0})
         , fstBest
         , sndBest
         , ((3, 'A'), Seat {ticket = Adult, distanceFromMid = 1.4142135623730951})
         , ((3, 'B'), Seat {ticket = Adult, distanceFromMid = 1.0})
         , ((3, 'C'), Seat {ticket = Child, distanceFromMid = 1.4142135623730951})
         ]
      in do
       mapList `shouldBe` seatList
       findBest 2 seatList `shouldBe` [fstBest, sndBest]

   it "can find the best 3 seats when no 3 contiguous seats exists" $
     let
       auditorium =
         -- ABCD
         [ ".AAA"
         , ".CCS"
         , "AAC."
         ]
       expected = [ (1, 'A'), (2, 'A'), (3, 'D') ]
      in assertCanFind 3 auditorium expected

   it "can find the best 3 seats when no 3 contiguous seats exists" $
     let
       auditorium =
         -- ABC
         [ ".AA"
         , ".CC"
         ]
      in assertCanFind 3 auditorium []

   it "can break a tie between rows" $
     let
       auditorium1 =
         -- ABC
         [ ".AA"
         , ".CC"
         , ".CC"
         , ".CC"
         ]
       auditorium2 =
         -- ABC
         [ ".AA"
         , "ACC"
         , "ACC"
         , ".CC"
         ]
      in do
       assertCanFind 2 auditorium1 [(2, 'A'), (3, 'A')]
       assertCanFind 2 auditorium2 [(1, 'A'), (4, 'A')]

   it "can break a tie between columns" $ do
    assertCanFind 1 [".."] [(1, 'A')]
    assertCanFind 2 ["..."] [(1, 'A'), (1, 'B')]
    assertCanFind 2 ["A..S..A"] [(1, 'B'), (1, 'C')]

   it "handles boundaries" $ do
     assertCanFind 1 ["."] [(1, 'A')]
     assertCanFind 2 ["."] []
     assertCanFind 1 [] []
     assertCanFind 0 [] []
     assertCanFind 2 [] []

  describe "find best in A1" $ do
   it "can find 2 best: 3L, 3M" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRST
         [ ".AAAAAAAA....AAAAAA."
         , ".CCS..CSS.A.AAAA.AC."
         , "AAC.SS.ACCC..AACCSS."
         , ".ACCSSAACS.S.ASAAAS."
         , ".AA.SS.CC....S.A.SSA"
         ]
       expected = [(3, 'L'), (3, 'M')]
      in assertCanFind 2 auditorium expected

   it "can break tie between 2J and 4K" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRST
         [ ".AAAAAAAA....AAAAAA."
         , ".CCS..CSS.A.AAAA.AC."
         , "AAC.SS.ACCCSAAACCSS."
         , ".ACCSSAACS.S.ASAAAS."
         , ".AA.SS.CC....S.A.SSA"
         ]
       expected = [(2, 'J')]
      in assertCanFind 1 auditorium expected

   it "can find 2 best: 1J, 1K" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRST
         [ ".AAAAAAAA....AAAAAA."
         , ".CCS..CSSAA.AAAA.AC."
         , "AAC.SS.ACCCSAAACCSS."
         , ".ACCSSAACS.S.ASAAAS."
         , ".AA.SS.CC....S.A.SSA"
         ]
       expected = [(1, 'J'), (1, 'K')]
      in assertCanFind 2 auditorium expected

   it "can find 2 best: 5J, 5K" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRST
         [ ".AAAAAAAAAA..AAAAAA."
         , ".CCS..CSSAA.AAAA.AC."
         , "AAC.SS.ACCCSAAACCSS."
         , ".ACCSSAACS.S.ASAAAS."
         , ".AA.SS.CC....S.A.SSA"
         ]
       expected = [(5, 'J'), (5, 'K')]
      in assertCanFind 2 auditorium expected

   it "can find 1 best: 4K" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRST
         [ ".AAAAAAAAAA..AAAAAA."
         , ".CCS..CSSAA.AAAA.AC."
         , "AAC.SS.ACCCSAAACCSS."
         , ".ACCSSAACS.S.ASAAAS."
         , ".AA.SS.CCAA..S.A.SSA"
         ]
       expected = [(4, 'K')]
      in assertCanFind 1 auditorium expected

  describe "find best in A2" $ do
   it "can find 2 best: 3G, 3H" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNO
         [ "S..A.A.CC....S."
         , "ACAS..ACS.A.AS."
         , "S.S..A..AC..AA."
         , ".SS.AA..CCS..S."
         , ".AC.AS.AA....A."
         ]
       expected = [(3, 'G') , (3, 'H')]
      in assertCanFind 2 auditorium expected

   it "can find 2 best: 4G, 4H" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNO
         [ "S..A.A.CC....S."
         , "ACAS..ACS.A.AS."
         , "S.S..AAAAC..AA."
         , ".SS.AA..CCS..S."
         , ".AC.AS.AA....A."
         ]
       expected = [(4, 'G') , (4, 'H')]
      in assertCanFind 2 auditorium expected

   it "can find 2 best: 2E, 2F" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNO
         [ "S..A.A.CC....S."
         , "ACAS..ACS.A.AS."
         , "S.S..AAAAC..AA."
         , ".SS.AACCCCS..S."
         , ".AC.AS.AA....A."
         ]
       expected = [(2, 'E') , (2, 'F')]
      in assertCanFind 2 auditorium expected

   it "can find 3 best: 1J, 1K, 1L" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNO
         [ "S..A.A.CC....S."
         , "ACASSSACS.A.AS."
         , "S.S..ACAAC..AA."
         , ".SS.AACCCCS..S."
         , ".AC.AS.AA....A."
         ]
       expected = [(1, 'J'), (1, 'K'), (1, 'L')]
      in assertCanFind 3 auditorium expected

  describe "find best in A3" $ do
   it "can find 3 best: 1K, 1L, 1M" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRSTUVWXYZ
         [ ".AAA.A..A....AA.AA.A.AA..."
         , "C..C..CCC.C.C..C..CCC....C"
         , "SSS.SS.SS...S..S.SSSSSSSSS"
         ]
       expected = [(1, 'K'), (1, 'L'), (1, 'M')]
      in assertCanFind 3 auditorium expected

   it "can find 3 best: 3J, 3K, 3L" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRSTUVWXYZ
         [ ".AAA.A..A.AACAA.AA.A.AA..."
         , "C..C..CCC.C.C..C..CCC....C"
         , "SSS.SS.SS...S..S.SSSSSSSSS"
         ]
       expected = [(3, 'J'), (3, 'K'), (3, 'L')]
      in assertCanFind 3 auditorium expected

   it "can find 3 best: 2V, 2W, 2X" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRSTUVWXYZ
         [ ".AAA.A..A.AACAA.AA.A.AA..."
         , "C..C..CCC.C.C..C..CCC....C"
         , "SSS.SS.SSCCSS..S.SSSSSSSSS"
         ]
       expected = [(2, 'V'), (2, 'W'), (2, 'X')]
      in assertCanFind 3 auditorium expected

   it "can find 3 best: 1X, 1Y, 1Z" $
     let
       auditorium =
         -- ABCDEFGHIJKLMNOPQRSTUVWXYZ
         [ ".AAA.A..A.AACAA.AA.A.AA..."
         , "C..C..CCC.C.C..C..CCCASS.C"
         , "SSS.SS.SSCCSS..S.SSSSSSSSS"
         ]
       expected = [(1, 'X'), (1, 'Y'), (1, 'Z')]
      in assertCanFind 3 auditorium expected

  describe "map to text" $
   it "can turn a map into text" $
     let a = mkAuditorium [ ".AA" , "C.." , "SSS"]
         expected = "  ABC\n1 .##\n2 #..\n3 ###\n"
      in mapToText a `shouldBe` expected

-- | Helper function to assert we can find the expected best seats.
assertCanFind :: Int -> [Text] -> [RowCol] -> Expectation
assertCanFind n auditorium expected =
  let mapList = Map.toList $ auditoriumMap $ mkAuditorium auditorium
  in do
    shuffledList <- Q.generate $ Q.shuffle mapList
    let actual = fst <$> findBest n shuffledList
     in actual `shouldBe` expected
