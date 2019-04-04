{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module UUIDs
( gen
) where

import Prelude
import Control.Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Csv as Csv
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID_V4
import GHC.Generics

data UuidRecord = UuidRecord { uuid :: String }
    deriving (Generic, Show)

instance Csv.ToRecord UuidRecord

csvEncodeToFile :: Csv.ToRecord a => FilePath -> [a] -> IO ()
csvEncodeToFile filePath as =
  ByteString.writeFile filePath (Csv.encode as) 

gen :: FilePath -> Int -> IO ()
gen filePath nr = do
  uuids <- replicateM nr (UuidRecord <$> UUID.toString <$> UUID_V4.nextRandom)
  csvEncodeToFile filePath uuids

