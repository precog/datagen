module Main where

import Options.Applicative
import qualified Opts
import qualified UUIDs

main :: IO ()
main = do
  opts <- execParser Opts.opts
  UUIDs.gen (Opts.to opts) (Opts.nr opts)
