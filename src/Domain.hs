{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain where

import Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTp
import qualified Data.ByteString.Lazy as ByteString
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified GHC.Generics as G

data FileWriteMode = Append | Overwrite deriving (Show)

writeFile :: FileWriteMode -> (FilePath -> ByteString.ByteString -> IO ())
writeFile Append = ByteString.appendFile
writeFile Overwrite = ByteString.writeFile

newtype Hour = Hour Int deriving (Eq, Ord, Show, G.Generic)
instance Aeson.ToJSON Hour
instance Aeson.ToJSONKey Hour where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . show . hourToInt)

hourToInt :: Hour -> Int
hourToInt (Hour i) = i

newtype Count = Count Int deriving (Eq, Ord, G.Generic)
instance Aeson.ToJSON Count

countToInt :: Count -> Int
countToInt (Count i) = i

data Campaign = Campaign
  { id :: Int
  , campaign :: UUID
  } deriving (Eq, Ord, G.Generic)
instance Aeson.ToJSON Campaign
instance Aeson.ToJSONKey Campaign where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . UUID.toString . campaign)

campaignId :: Campaign -> Int
campaignId (Campaign i _) = i

data Event = Event
  { id :: Int
  , event :: String
  } deriving (Eq, Ord, Show, G.Generic)
instance Aeson.ToJSON Event
instance Aeson.ToJSONKey Event where
  toJSONKey = AesonTp.toJSONKeyText (Text.pack . event)

eventId :: Event -> Int
eventId (Event i _) = i

newtype EventCounts = EventCounts (Map Event Count) deriving (G.Generic)
instance Aeson.ToJSON EventCounts

newtype HourCounts = HourCounts (Map Hour EventCounts) deriving (G.Generic)
instance Aeson.ToJSON HourCounts

newtype CampaignCounts = CampaignCounts (Map Campaign HourCounts) deriving (G.Generic)
instance Aeson.ToJSON CampaignCounts
