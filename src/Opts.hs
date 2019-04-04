{-# LANGUAGE NoImplicitPrelude #-}

module Opts where

import Prelude
import Options.Applicative

data GenUuidOptions = GenUuidOptions
  { nr :: Int 
  , to :: FilePath
  }

nrOpt :: Parser Int
nrOpt = option auto
  (  long "number"
  <> short 'n' 
  <> help "Number of UUIDs to generate"
  <> showDefault
  <> value 100
  <> metavar "INT" )

toOpt :: Parser FilePath
toOpt = strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> showDefault
  <> value "campaigns.csv"
  <> help "Output file" )
  
genUuidOpts :: Parser GenUuidOptions
genUuidOpts = GenUuidOptions <$> nrOpt <*> toOpt

opts :: ParserInfo GenUuidOptions
opts = info genUuidOpts
  (  fullDesc
  <> progDesc "Little tool to generate data"
  <> header "datagen" 
  )
