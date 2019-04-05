{-# LANGUAGE NoImplicitPrelude #-}

module Gen
( gen
, genUuids
) where

import Prelude
import qualified Control.Monad.Random as R
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID_V4

import Csv (csvEncodeToFile)
import qualified Domain as D
import qualified DomainCsv as Csv
import qualified Opts
import qualified Rnd

ldjsonEncodeToFile :: Aeson.ToJSON a => FilePath -> [a] -> IO ()
ldjsonEncodeToFile filePath as =
  ByteString.writeFile filePath (BSC.unlines $ map Aeson.encode as)

includeId :: [Int -> a] -> [a]
includeId f = (\(p1, p2) -> p1 p2) <$> zip f [0..]

mkCsvEvents :: (Int, D.EventCounts) -> [Int -> Csv.Event]
mkCsvEvents (rowIndex, (D.EventCounts m)) = 
  mk <$> Map.toList m
  where
    mk :: (D.Event, D.CampaignCounts) -> (Int -> Csv.Event)
    mk (e, _) = \i -> Csv.Event i rowIndex (D.event e)

mkCsvCampaigns :: (Int, D.EventCounts) -> [Int -> Csv.Campaign]
mkCsvCampaigns (rowIndex, (D.EventCounts m)) = 
  Map.toList m >>= mk
  where
    mk :: (D.Event, D.CampaignCounts) -> [Int -> Csv.Campaign]
    mk (e, (D.CampaignCounts m)) = 
      (map (\(c, _) -> (e, c)) $ Map.toList m) >>= (pure . mk1)
    mk1 :: (D.Event, D.Campaign) -> (Int -> Csv.Campaign)
    mk1 (e, c) = (\i -> Csv.Campaign i rowIndex (D.eventId e) (UUID.toString (D.campaign c)))

mkCsvHourCounts :: (Int, D.EventCounts) -> [Int -> Csv.HourCount]
mkCsvHourCounts (rowIndex, (D.EventCounts m)) = 
  Map.toList m >>= mk
  where
    mk :: (D.Event, D.CampaignCounts) -> [Int -> Csv.HourCount]
    mk (e, (D.CampaignCounts m)) = 
      (map (\(c, hc) -> (e, c, hc)) $ Map.toList m) >>= mk0
    mk0 :: (D.Event, D.Campaign, D.HourCounts) -> [Int -> Csv.HourCount]
    mk0 (e, c, (D.HourCounts m)) =  
      (map (\(h, cnt) -> (e, c, h, cnt)) $ Map.toList m) >>= (pure . mk1)
    mk1 :: (D.Event, D.Campaign, D.Hour, D.Count) -> (Int -> Csv.HourCount)
    mk1 (e, c, h, cnt) = (\i -> Csv.HourCount i rowIndex (D.eventId e) (D.campaignId c) (D.hourToInt h) (D.countToInt cnt))

gen :: Opts.GenOptions -> NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> IO ()
gen (Opts.GenOptions nr) cs es = do
  eventCounts <- R.evalRandIO $ R.replicateM nr $ Rnd.randomEventCounts cs es
  ldjsonEncodeToFile "./eventCounts.ldjson" eventCounts
  let indexedEventCounts = zip [0..] eventCounts
  csvEncodeToFile "./events.csv" (includeId $ indexedEventCounts >>= mkCsvEvents)
  csvEncodeToFile "./eventCampaigns.csv" (includeId $ indexedEventCounts >>= mkCsvCampaigns)
  csvEncodeToFile "./hourCounts.csv" (includeId $ indexedEventCounts >>= mkCsvHourCounts)

genUuids :: Opts.GenUuidOptions -> IO ()
genUuids (Opts.GenUuidOptions nr) = do
  uuids <- R.replicateM nr (Csv.UuidRecord <$> UUID.toString <$> UUID_V4.nextRandom)
  csvEncodeToFile "./campaigns.csv" uuids
  