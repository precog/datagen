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

randomHourCounts :: RandomGen g => NEL.NonEmpty D.Event -> Rand g D.HourCounts
randomHourCounts es = do
  nrElems <- getRandomR (1, 23)
  mk <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomHour <*> randomEventCounts es
    mk :: [(D.Hour, D.EventCounts)] -> D.HourCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.Hour], [D.EventCounts]) -> D.HourCounts
    mk0 (hs, ecs) = mkHourCounts $ zip hs ecs
    mkHourCounts = D.HourCounts . Map.fromList

randomCampaignCounts :: RandomGen g => NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.CampaignCounts
randomCampaignCounts cs es = do
  nrElems <- getRandomR (1, length cs - 1)
  mk <$> replicateM nrElems randomPair
  where
    randomPair :: RandomGen g => Rand g (D.Campaign, D.HourCounts)
    randomPair = (,) <$> randomFromNEL cs <*> randomHourCounts es
    mk :: [(D.Campaign, D.HourCounts)] -> D.CampaignCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.Campaign], [D.HourCounts]) -> D.CampaignCounts
    mk0 (cs, hcs) = D.CampaignCounts $ Map.fromList $ zip cs hcs
