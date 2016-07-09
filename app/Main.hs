module Main where

import System.Environment (lookupEnv, getArgs)
import Lib

main :: IO ()
main = do
  token <- lookupEnv "GITHUB_TOKEN"
  args <- getArgs
  repos <- someFunc token args
  print repos
