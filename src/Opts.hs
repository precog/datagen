{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Opts where

import Prelude
import Options.Applicative

data Options = Gen GenOptions | GenUuid GenUuidOptions

newtype GenOptions =
  GenOptions
    { nr :: Int }

newtype GenUuidOptions =
  GenUuidOptions
    { nr :: Int }

nrOpt :: Parser Int
nrOpt = option auto
  (  long "number"
  <> short 'n'
  <> help "Number of rows to generate"
  <> showDefault
  <> value 100
  <> metavar "INT" )

genUuidOpts0 :: Parser GenUuidOptions
genUuidOpts0 = GenUuidOptions <$> nrOpt

genUuidOpts :: Parser GenUuidOptions
genUuidOpts = hsubparser $
  command "genuuids" (info genUuidOpts0 ( progDesc "Generates a number of UUIDs and writes them to file" ))

genOpts0 :: Parser GenOptions
genOpts0 = GenOptions <$> nrOpt

genOpts :: Parser GenOptions
genOpts = hsubparser $
  command "gen" (info genOpts0 ( progDesc "Generates ldjson and csv files" ))

parser :: Parser Options
parser = (GenUuid <$> genUuidOpts) <|> (Gen <$> genOpts)

opts :: ParserInfo Options
opts = info (parser <**> helper)
  (  fullDesc
  <> progDesc "Little tool to generate data"
  <> header "datagen"
  )
