{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec where

import           RIO
import           Test.Hspec
import           TicketSystem (theAnswer)

main :: IO ()
main = hspec $
  describe "the answer to everything" $
   it "can give the answer" $
    theAnswer `shouldBe` 42
