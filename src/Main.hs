{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO
import           Say (say)

main :: IO ()
main = say "Hello, Haskell!"