{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain where

import Prelude
import Data.Map.Strict (Map)
import Numeric (showFFloat)
import Text.Printf (printf)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTp
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.Csv as Csv
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified GHC.Generics as G

data FileWriteMode = Append | Overwrite deriving (Show)

writeFile :: FileWriteMode -> (FilePath -> BSL.ByteString -> IO ())
writeFile Append = BSL.appendFile
writeFile Overwrite = BSL.writeFile

newtype MinuteOfDay = MinuteOfDay Int deriving (Eq, Ord, Show, G.Generic)

instance Aeson.ToJSON MinuteOfDay

instance Aeson.ToJSONKey MinuteOfDay where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . minuteOfDayToString)

minuteOfDayToString :: MinuteOfDay -> String
minuteOfDayToString (MinuteOfDay i) =
  printf "%02d:%02d" (div i 60) (rem i 60)

newtype Count = Count Int deriving (Eq, Ord, G.Generic, Show)
instance Aeson.ToJSON Count

countToInt :: Count -> Int
countToInt (Count i) = i

data Campaign = Campaign
  { campaignUuid :: UUID
  , successRate :: Double
  } deriving ( Eq
             , Ord
             , G.Generic
             , Show
             )

instance Aeson.ToJSON Campaign

instance Aeson.ToJSONKey Campaign where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . UUID.toString . unUuid . campaignUuid)

instance Csv.FromRecord Campaign

instance Csv.ToRecord Campaign where
  toRecord (Campaign uuid' successRate') =
    Csv.record
      [ Csv.toField uuid'
      , Csv.toField (showFFloat Nothing successRate' "")
      ]

newtype UUID = UUID { unUuid :: UUID.UUID }
  deriving ( Eq
           , G.Generic
           , Ord
           , Show
           )

instance Aeson.ToJSON UUID

instance Csv.ToField UUID where
  toField = BSUTF8.fromString . UUID.toString . unUuid

instance Csv.FromField UUID where
  parseField :: BS.ByteString -> Csv.Parser UUID
  parseField bs = case UUID.fromString $ BSUTF8.toString bs of
    Nothing -> fail $ "Invalid UUID: " <>  BSUTF8.toString bs
    Just uuid -> pure $ UUID uuid


newtype Event = Event
  { event :: String
  } deriving ( Eq
             , Ord
             , Show
             , G.Generic
             )

instance Aeson.ToJSON Event

instance Aeson.ToJSONKey Event where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . event)

instance Csv.FromRecord Event

instance Csv.ToRecord Event

newtype EventCounts = EventCounts (Map Event Count)
  deriving ( G.Generic
           , Show
           )

instance Aeson.ToJSON EventCounts

newtype MinuteOfDayCounts = MinuteOfDayCounts (Map MinuteOfDay EventCounts)
  deriving ( G.Generic
           , Show
           )

instance Aeson.ToJSON MinuteOfDayCounts

newtype CampaignCounts = CampaignCounts (Map Campaign MinuteOfDayCounts)
  deriving ( G.Generic
           , Show
           )

instance Aeson.ToJSON CampaignCounts

data Row = Row
  { date :: String
  , stats :: CampaignCounts
  } deriving ( G.Generic )

instance Aeson.ToJSON Row
