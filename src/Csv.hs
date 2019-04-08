{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Csv where

import Prelude
import qualified Control.Monad.Except as ME
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import System.IO.Error (userError)

import qualified Domain as D

csvEncodeToFile :: Csv.ToRecord a => D.FileWriteMode -> FilePath -> [a] -> IO ()
csvEncodeToFile mode filePath as =
  (D.writeFile mode) filePath (Csv.encode as)

csvEncodeNamedToFile :: (Csv.DefaultOrdered a, Csv.ToNamedRecord a) => D.FileWriteMode -> FilePath -> [a] -> IO ()
csvEncodeNamedToFile mode filePath as =
  (D.writeFile mode) filePath (Csv.encodeDefaultOrderedByName as)

csvDecodeFromFile :: Csv.FromRecord a => FilePath -> IO (Vector a)
csvDecodeFromFile filePath = do
  d <- Csv.decode Csv.NoHeader <$> ByteString.readFile filePath
  either (ME.throwError . userError) pure d
