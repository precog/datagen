{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DomainCsv where

import Prelude
import qualified Data.Csv as Csv
import qualified GHC.Generics as G

data UuidRecord = UuidRecord
  { uuid :: String
  } deriving (Show, G.Generic)

instance Csv.ToRecord UuidRecord

data Event = Event
  { id :: Int
  , row_index :: Int
  , event :: String
  } deriving (Show, G.Generic)

instance Csv.DefaultOrdered Event
instance Csv.ToNamedRecord Event

data Campaign = Campaign
  { id :: Int
  , row_index :: Int
  , event_id :: Int
  , campaign :: String
  } deriving (Show, G.Generic)

instance Csv.DefaultOrdered Campaign
instance Csv.ToNamedRecord Campaign

data HourCount = HourCount
  { id :: Int
  , row_index :: Int
  , event_id :: Int
  , campaign_id :: Int
  , hour :: Int
  , count :: Int
  } deriving (Show, G.Generic)

instance Csv.DefaultOrdered HourCount
instance Csv.ToNamedRecord HourCount
