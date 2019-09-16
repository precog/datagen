{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude
import Csv (csvDecodeFromFile)
import Data.Functor ((<&>))
import System.IO.Error (userError)
import qualified Control.Monad.Except as ME
import qualified Data.List.NonEmpty as NEL
import qualified Data.Vector as Vector
import qualified Domain as D
import qualified Gen
import qualified Options.Applicative as OA
import qualified Opts

exec :: Opts.Options -> IO ()
exec (Opts.GenUuid o) = Gen.genUuids o
exec (Opts.Gen o) = do
  cs <- decodeCampaignsFrom "./campaigns.csv"
  es <- decodeEventsFrom "./resources/events.csv"
  Gen.gen o cs es
  where
    decodeCampaignsFrom :: FilePath -> IO (NEL.NonEmpty D.Campaign)
    decodeCampaignsFrom path =
      csvDecodeFromFile path
        <&> (NEL.nonEmpty . Vector.toList)
        >>= maybe (ME.throwError $ userError "No campaigns found") pure

    decodeEventsFrom :: FilePath -> IO (NEL.NonEmpty D.Event)
    decodeEventsFrom path =
      csvDecodeFromFile path
      <&> (NEL.nonEmpty . Vector.toList)
      >>= maybe (ME.throwError $ userError "No events found") pure


main :: IO ()
main = OA.execParser Opts.opts >>= exec
