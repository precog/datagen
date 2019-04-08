{-# LANGUAGE NoImplicitPrelude #-}

module Rnd where

import Prelude
import Control.Monad.Random (Rand, RandomGen, getRandomR, replicateM)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Domain as D

randomHour :: RandomGen g => Rand g D.Hour
randomHour = D.Hour <$> getRandomR (0, 23)

randomCount :: RandomGen g => D.Count -> Rand g D.Count
randomCount (D.Count max) = D.Count <$> getRandomR (0, max)

randomFromList :: RandomGen g => [a] -> Rand g (Maybe a)
randomFromList [] = pure Nothing
randomFromList l = do
  i <- getRandomR (0, length l - 1)
  pure $ Just $ l !! i

randomFromNEL :: RandomGen g => NEL.NonEmpty a -> Rand g a
randomFromNEL l@(h :| t) = Maybe.fromMaybe h <$> randomFromList (NEL.toList l)

randomHourCountPair :: RandomGen g => Rand g ((D.Hour, D.Count), (D.Hour, D.Count))
randomHourCountPair = do
  h <- randomHour
  sent <- randomCount $ D.Count 1000
  errored <- randomCount sent
  pure ((h, sent), (h, errored))

-- Currently the following functions favor smaller amounts because
-- duplicates in List are simply removed when they are stored in the Map
randomHourCountsPair :: RandomGen g => Rand g (D.HourCounts, D.HourCounts)
randomHourCountsPair = do
  nrElems <- getRandomR (1, 23)
  mk <$> replicateM nrElems randomHourCountPair
  where
    mk = (\(x, y) -> (mkHourCounts x, mkHourCounts $ filter (\p -> snd p > D.Count 0) $ y)) <$> unzip
    mkHourCounts = D.HourCounts . Map.fromList

randomCampaignCountsPair :: RandomGen g => NEL.NonEmpty D.Campaign -> Rand g (D.CampaignCounts, D.CampaignCounts)
randomCampaignCountsPair cs = do
  nrElems <- getRandomR (1, length cs - 1)
  mk <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomFromNEL cs <*> randomHourCountsPair
    mk :: [(D.Campaign, (D.HourCounts, D.HourCounts))] -> (D.CampaignCounts, D.CampaignCounts)
    mk = mk0 <$> (\(x, y) -> (x, unzip y)) <$> unzip
    mk0 :: ([D.Campaign], ([D.HourCounts], [D.HourCounts])) -> (D.CampaignCounts, D.CampaignCounts)
    mk0 (cs, (hcs1, hcs2)) = (mkCampaignCounts $ zip cs hcs1, mkCampaignCounts $ zip cs hcs2)
    mkCampaignCounts = D.CampaignCounts . Map.fromList

randomEventCounts :: RandomGen g => NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.EventCounts
randomEventCounts cs es = do
  nrElems <- getRandomR (1, length es - 1)
  mk <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomFromNEL es <*> randomCampaignCountsPair cs
    mk :: [(D.Event, (D.CampaignCounts, D.CampaignCounts))] -> D.EventCounts
    mk = mk0 <$> (\(x, y) -> (x, unzip y)) <$> unzip
    mk0 :: ([D.Event], ([D.CampaignCounts], [D.CampaignCounts])) -> D.EventCounts
    mk0 (es, (ccs1, ccs2)) = D.EventCounts $
      Map.union
        (mkMap sent $ zip es ccs1)
        (mkMap errored $ zip es ccs2)
    mkMap :: (D.Event -> D.Event) -> [(D.Event, D.CampaignCounts)] -> Map D.Event D.CampaignCounts
    mkMap f = ((Map.mapKeys f) . Map.fromList)
    sent (D.Event i n) = D.Event i (n ++ "_sent")
    errored (D.Event i n) = D.Event (i + (max 100 (length es))) (n ++ "_errored")
