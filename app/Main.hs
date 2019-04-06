{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude
import qualified Control.Monad.Except as ME
import qualified Data.List.NonEmpty as NEL
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Options.Applicative as OA
import System.IO.Error (userError)

import Csv (csvDecodeFromFile)
import qualified Domain as D
import qualified Gen
import qualified Opts

exec :: Opts.Options -> IO()
exec (Opts.GenUuid o) = Gen.genUuids o
exec (Opts.Gen o) = do
  cs <- csvDecodeFromFile "./campaigns.csv" >>= mkCampaigns
  es <- csvDecodeFromFile "./resources/events.csv" >>= mkEvents
  Gen.gen o cs es
  where
    mkCampaigns :: Vector [String] -> IO (NEL.NonEmpty D.Campaign)
    mkCampaigns sss = do
      cs <- mkCampaigns0 (ME.join $ Vector.toList sss)
      maybe (ME.throwError $ userError "No campaigns found") pure $ NEL.nonEmpty cs
    mkCampaigns0 :: [String] -> IO [D.Campaign]
    mkCampaigns0 ss = traverse (\(f, s) -> mkCampaign f s) $ zip [0..] ss
    mkCampaign :: Int -> String -> IO D.Campaign
    mkCampaign i s =
      maybe (ME.throwError $ userError $ "Error parsing UUID: " ++ s) pure $ D.Campaign i <$> UUID.fromString s
    mkEvents :: Vector [String] -> IO (NEL.NonEmpty D.Event)
    mkEvents sss =
      maybe (ME.throwError $ userError "No events found") pure $ NEL.nonEmpty (mkEvents0 (ME.join $ Vector.toList sss))
    mkEvents0 :: [String] -> [D.Event]
    mkEvents0 ss = map (\(f, s) -> D.Event f s) $ zip [0..] ss

main :: IO ()
main = OA.execParser Opts.opts >>= exec
