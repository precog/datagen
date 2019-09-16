{-# LANGUAGE NoImplicitPrelude #-}

module Rnd where

import Prelude
import qualified Control.Monad.Random as Rand
import Control.Monad.Random (Rand, RandomGen)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Domain as D
import Data.Functor ((<&>))

randomMinuteOfDay :: RandomGen g => Rand g D.MinuteOfDay
randomMinuteOfDay = D.MinuteOfDay <$> Rand.getRandomR (0, (24 * 60) - 1)

randomFromList :: RandomGen g => [a] -> Rand g (Maybe a)
randomFromList [] = pure Nothing
randomFromList l = do
  i <- Rand.getRandomR (0, length l - 1)
  pure $ Just $ l !! i

randomFromNEL :: RandomGen g => NEL.NonEmpty a -> Rand g a
randomFromNEL l@(h :| t) = Maybe.fromMaybe h <$> randomFromList (NEL.toList l)

randErrCountFromTotal :: (RandomGen g) => Int -> Double -> Rand g Int
randErrCountFromTotal numEvts successRate = do
  -- add or subtract 1-3 std deviations from the average using the 68–95–99.7 rule as a heuristic
  numStdDeviations <- Rand.fromList [(1.0, 0.6827), (2.0, 0.2718), (3.0, 0.0455)]
  let
    stdDeviation = binomialStdDeviation numEvts successRate
    deviationMin = round $ (numStdDeviations * stdDeviation) - stdDeviation
    deviationMax = round $ numStdDeviations * stdDeviation
    avgSuccessCount = round $ realToFrac numEvts * successRate

  Rand.fromList [((+), 1/2), ((-), 1/2)]
    <*> pure avgSuccessCount
    <*> Rand.getRandomR (deviationMin, deviationMax)
    <&> (clamp 0 numEvts)
    <&> (\successCount -> numEvts - successCount)
  where
    clamp :: (Ord a) => a -> a -> a -> a
    clamp low high n =
      max low (min high n)

    binomialStdDeviation :: Int -> Double -> Double
    binomialStdDeviation nrTrials successProbability =
      sqrt $ (realToFrac nrTrials) * successProbability * (1 - successProbability)

randomEventCountPair ::
  RandomGen g
  => D.Campaign
  -> NEL.NonEmpty D.Event
  -> Rand g ((D.Event, D.Count), (D.Event, D.Count))
randomEventCountPair c es = do
  evt <- randomFromNEL es
  nrSent <- Rand.getRandomR (0, 1000)
  nrErrored <- randErrCountFromTotal nrSent (D.successRate c)
  pure ((sent evt, D.Count nrSent), (errored evt, D.Count nrErrored))
  where
    sent (D.Event name) = D.Event (name ++ "_sent")
    errored (D.Event name) = D.Event (name ++ "_errored")

-- Currently the following functions favor smaller amounts because
-- duplicates in List are simply removed when they are stored in the Map
randomEventCounts :: RandomGen g => D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.EventCounts
randomEventCounts c es = do
  nrElems <- Rand.getRandomR (1, length es - 1)
  mk <$> Rand.replicateM nrElems (randomEventCountPair c es)
  where
    mk :: [((D.Event, D.Count), (D.Event, D.Count))] -> D.EventCounts
    mk = (\(x, y) -> mkEventCounts $ x ++ filter (\p -> snd p > D.Count 0) y) <$> unzip
    mkEventCounts = D.EventCounts . Map.fromList

randomMinuteOfDayCounts :: RandomGen g => D.Campaign -> NEL.NonEmpty D.Event -> Rand g D.MinuteOfDayCounts
randomMinuteOfDayCounts c es = do
  nrElems <- Rand.getRandomR (1, 24 * 60)
  mk <$> Rand.replicateM nrElems randomPair
  where
    randomPair = (,) <$> randomMinuteOfDay <*> randomEventCounts c es
    mk :: [(D.MinuteOfDay, D.EventCounts)] -> D.MinuteOfDayCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.MinuteOfDay], [D.EventCounts]) -> D.MinuteOfDayCounts
    mk0 (mods, ecs) = mkMinuteOfDayCounts $ zip mods ecs
    mkMinuteOfDayCounts = D.MinuteOfDayCounts . Map.fromList

randomCampaignCounts ::
  RandomGen g
  => NEL.NonEmpty D.Campaign
  -> NEL.NonEmpty D.Event
  -> Rand g D.CampaignCounts
randomCampaignCounts cs es = do
  nrElems <- Rand.getRandomR (1, length cs - 1)
  mk <$> Rand.replicateM nrElems randomPair
  where
    randomPair :: RandomGen g => Rand g (D.Campaign, D.MinuteOfDayCounts)
    randomPair = do
      c <- randomFromNEL cs
      mdcs <-  randomMinuteOfDayCounts c es
      pure (c, mdcs)
    mk :: [(D.Campaign, D.MinuteOfDayCounts)] -> D.CampaignCounts
    mk = mk0 <$> unzip
    mk0 :: ([D.Campaign], [D.MinuteOfDayCounts]) -> D.CampaignCounts
    mk0 (cs, modcs) = D.CampaignCounts $ Map.fromList $ zip cs modcs
