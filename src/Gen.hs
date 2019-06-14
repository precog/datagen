{-# LANGUAGE NoImplicitPrelude #-}

module Gen
( gen
, genUuids
) where

import Prelude
import qualified Control.Monad.Random as R
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID_V4

import Csv (csvEncodeNamedToFile, csvEncodeToFile)
import qualified Domain as D
import qualified DomainCsv as Csv
import qualified Opts
import qualified Rnd

ldjsonEncodeToFile :: Aeson.ToJSON a => D.FileWriteMode -> FilePath -> [a] -> IO ()
ldjsonEncodeToFile mode filePath as =
  (D.writeFile mode) filePath (BSC.unlines $ map Aeson.encode as)

includeId :: Int -> [Int -> a] -> [a]
includeId startIndex f = (\(p1, p2) -> p1 p2) <$> zip f [startIndex..]

mkRows :: [D.CampaignCounts] -> [Int -> D.Row]
mkRows ccs =
  mk <$> ccs
  where
    mk cc = \i -> D.Row (toDate i) cc
    toDate i = Time.showGregorian (Time.addDays (toInteger i) (Time.fromGregorian 2019 1 1))

-- mkCsvEvents :: (Int, D.EventCounts) -> [Int -> Csv.Event]
-- mkCsvEvents (rowIndex, (D.EventCounts m)) =
--   mk <$> Map.toList m
--   where
--     mk :: (D.Event, D.CampaignCounts) -> (Int -> Csv.Event)
--     mk (e, _) = \i -> Csv.Event i rowIndex (D.event e)

-- mkCsvCampaigns :: (Int, D.EventCounts) -> [Int -> Csv.Campaign]
-- mkCsvCampaigns (rowIndex, (D.EventCounts m)) =
--   Map.toList m >>= mk
--   where
--     mk :: (D.Event, D.CampaignCounts) -> [Int -> Csv.Campaign]
--     mk (e, (D.CampaignCounts m)) =
--       (map (\(c, _) -> (e, c)) $ Map.toList m) >>= (pure . mk1)
--     mk1 :: (D.Event, D.Campaign) -> (Int -> Csv.Campaign)
--     mk1 (e, c) = (\i -> Csv.Campaign i rowIndex (D.eventId e) (UUID.toString (D.campaign c)))

-- mkCsvHourCounts :: (Int, D.EventCounts) -> [Int -> Csv.HourCount]
-- mkCsvHourCounts (rowIndex, (D.EventCounts m)) =
--   Map.toList m >>= mk
--   where
--     mk :: (D.Event, D.CampaignCounts) -> [Int -> Csv.HourCount]
--     mk (e, (D.CampaignCounts m)) =
--       (map (\(c, hc) -> (e, c, hc)) $ Map.toList m) >>= mk0
--     mk0 :: (D.Event, D.Campaign, D.HourCounts) -> [Int -> Csv.HourCount]
--     mk0 (e, c, (D.HourCounts m)) =
--       (map (\(h, cnt) -> (e, c, h, cnt)) $ Map.toList m) >>= (pure . mk1)
--     mk1 :: (D.Event, D.Campaign, D.Hour, D.Count) -> (Int -> Csv.HourCount)
--     mk1 (e, c, h, cnt) = (\i -> Csv.HourCount i rowIndex (D.eventId e) (D.campaignId c) (D.hourToInt h) (D.countToInt cnt))

data ChunkArgs = ChunkArgs
  { startRow :: Int
  , startE :: Int
  , startEC :: Int
  , startHC :: Int
  , mode :: D.FileWriteMode
  } deriving (Show)

genChunk :: NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> Int -> ChunkArgs -> IO ChunkArgs
genChunk cs es nr (ChunkArgs startRow startE startEC startHC mode) = do
  campaignCounts <- R.evalRandIO $ R.replicateM nr $ Rnd.randomCampaignCounts cs es
  let rows = includeId startRow $ mkRows campaignCounts
  ldjsonEncodeToFile mode "./stats.ldjson" rows
  -- let indexedEventCounts = zip [startRow..] eventCounts
  -- let csvEvents = includeId startE $ indexedEventCounts >>= mkCsvEvents
  -- csvEncodeNamedToFile mode "./events.csv" csvEvents
  -- let csvEventCampaigns = includeId startEC $ indexedEventCounts >>= mkCsvCampaigns
  -- csvEncodeNamedToFile mode "./eventCampaigns.csv" csvEventCampaigns
  -- let csvHourCounts = includeId startHC $ indexedEventCounts >>= mkCsvHourCounts
  -- csvEncodeNamedToFile mode "./hourCounts.csv" csvHourCounts
  pure $ ChunkArgs (startRow + nr) 0 0 0 D.Append --(startE + length csvEvents) (startEC + length csvEventCampaigns) (startHC + length csvHourCounts) D.Append

gen :: Opts.GenOptions -> NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> IO ()
gen (Opts.GenOptions nr) cs es = do
  go nr (ChunkArgs 0 0 0 0 D.Overwrite)
  pure ()
  where
    go :: Int -> ChunkArgs -> IO ChunkArgs
    go nr is
      | nr <= 0 = pure is
      | nr <= maxChunkSize = step nr is
      | otherwise = do
          nextIs <- step maxChunkSize is
          go (nr - maxChunkSize) nextIs
    step = genChunk cs es
    maxChunkSize = 1000

genUuids :: Opts.GenUuidOptions -> IO ()
genUuids (Opts.GenUuidOptions nr) = do
  uuids <- R.replicateM nr (Csv.UuidRecord <$> UUID.toString <$> UUID_V4.nextRandom)
  csvEncodeToFile D.Overwrite "./campaigns.csv" uuids
