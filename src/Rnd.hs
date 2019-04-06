{-# LANGUAGE NoImplicitPrelude #-}

module Rnd where

import Prelude
import Control.Monad.Random (Rand, RandomGen, getRandomR, replicateM)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Domain as D

randomHour :: RandomGen g => Rand g D.Hour
randomHour = D.Hour <$> getRandomR (0, 23)

randomCount :: RandomGen g => Rand g D.Count
randomCount = D.Count <$> getRandomR (0, 1000)

randomPair :: RandomGen g => Rand g (D.Hour, D.Count)
randomPair = (,) <$> randomHour <*> randomCount

randomFromList :: RandomGen g => [a] -> Rand g (Maybe a)
randomFromList [] = pure Nothing
randomFromList l = do
  i <- getRandomR (0, length l - 1)
  pure $ Just $ l !! i

randomFromNEL :: RandomGen g => NEL.NonEmpty a -> Rand g a
randomFromNEL l@(h :| t) = Maybe.fromMaybe h <$> randomFromList (NEL.toList l)

nelLength :: NEL.NonEmpty a -> Int
nelLength (_ :| t) = 1 + length t

-- Currently the following functions favor smaller amounts because
-- duplicates in List are simply removed when they are stored in the Map
randomHourCounts :: RandomGen g => Rand g D.HourCounts
randomHourCounts = D.HourCounts <$> do
  nrElems <- getRandomR (1, 23)
  Map.fromList <$> replicateM nrElems randomPair

randomCampaignCounts :: RandomGen g => NEL.NonEmpty D.Campaign -> Rand g D.CampaignCounts
randomCampaignCounts cs = do
  nrElems <- getRandomR (1, nelLength cs - 1)
  D.CampaignCounts <$> Map.fromList <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomFromNEL cs <*> randomHourCounts

randomEventCounts :: RandomGen g => NEL.NonEmpty D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.EventCounts
randomEventCounts cs es = do
  nrElems <- getRandomR (1, length es - 1)
  D.EventCounts <$> Map.fromList <$> replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomFromNEL es <*> randomCampaignCounts cs
