{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO
import           RIO.List           as L
import           RIO.Text           as T
import           Say                (say, sayString)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "./test-cases/A1.txt" (L.headMaybe args)
  ls <- toStrippedLines <$> readFileUtf8 path
  mapM_ say ls
  sayString path

-- | Takes text and splits it into lines where there are new lines (Windows or
-- Unix line endings). In addition, each line is stripped (i.e. trimmed) of
-- white space at the beginning and end.
toStrippedLines :: Text -> [Text]
toStrippedLines = (T.strip <$>) . T.linesCR
