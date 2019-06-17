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


randomMinuteOfDay :: RandomGen g => Rand g D.MinuteOfDay
randomMinuteOfDay = D.MinuteOfDay <$> getRandomR (0, (24 * 60) - 1)

randomCount :: RandomGen g => D.Count -> Rand g D.Count
randomCount (D.Count max) = D.Count <$> getRandomR (0, max)

randomFromList :: RandomGen g => [a] -> Rand g (Maybe a)
randomFromList [] = pure Nothing
randomFromList l = do
  i <- getRandomR (0, length l - 1)
  pure $ Just $ l !! i

randomFromNEL :: RandomGen g => NEL.NonEmpty a -> Rand g a
randomFromNEL l@(h :| t) = Maybe.fromMaybe h <$> randomFromList (NEL.toList l)

randomEventCountPair :: RandomGen g => NEL.NonEmpty D.Event -> Rand g ((D.Event, D.Count), (D.Event, D.Count))
randomEventCountPair es = do
  nrSent <- randomCount $ D.Count 1000
  nrErrored <- randomCount nrSent
  e <- randomFromNEL es
  pure ((sent e, nrSent), (errored e, nrErrored))
  where
    sent (D.Event i n) = D.Event i (n ++ "_sent")
    errored (D.Event i n) = D.Event (i + (max 100 (length es))) (n ++ "_errored")

-- Currently the following functions favor smaller amounts because
-- duplicates in List are simply removed when they are stored in the Map
randomEventCounts :: RandomGen g => NEL.NonEmpty D.Event -> Rand g D.EventCounts
randomEventCounts es = do
  nrElems <- getRandomR (1, length es - 1)
  mk <$> replicateM nrElems (randomEventCountPair es)
  where
    mk :: [((D.Event, D.Count), (D.Event, D.Count))] -> D.EventCounts
    mk = (\(x, y) -> mkEventCounts $ x ++ filter (\p -> snd p > D.Count 0) y) <$> unzip
    mkEventCounts = D.EventCounts . Map.fromList

randomMinuteOfDayCounts :: RandomGen g => NEL.NonEmpty D.Event -> Rand g D.MinuteOfDayCounts
randomMinuteOfDayCounts es = do
  nrElems <- getRandomR (1, 24 * 60)
  mk <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomMinuteOfDay <*> randomEventCounts es
    mk :: [(D.MinuteOfDay, D.EventCounts)] -> D.MinuteOfDayCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.MinuteOfDay], [D.EventCounts]) -> D.MinuteOfDayCounts
    mk0 (mods, ecs) = mkMinuteOfDayCounts $ zip mods ecs
    mkMinuteOfDayCounts = D.MinuteOfDayCounts . Map.fromList

randomCampaignCounts :: RandomGen g => NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.CampaignCounts
randomCampaignCounts cs es = do
  nrElems <- getRandomR (1, length cs - 1)
  mk <$> replicateM nrElems randomPair
  where
    randomPair :: RandomGen g => Rand g (D.Campaign, D.MinuteOfDayCounts)
    randomPair = (,) <$> randomFromNEL cs <*> randomMinuteOfDayCounts es
    mk :: [(D.Campaign, D.MinuteOfDayCounts)] -> D.CampaignCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.Campaign], [D.MinuteOfDayCounts]) -> D.CampaignCounts
    mk0 (cs, modcs) = D.CampaignCounts $ Map.fromList $ zip cs modcs
