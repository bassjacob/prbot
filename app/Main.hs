module Main where

import System.Environment (getEnvironment, lookupEnv, getArgs)
import Lib

data Opts = Opts {
                 }

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  token <- lookupEnv "GITHUB_TOKEN"

  repos <- getUserRepos token args
  print $ "Finding PRs for " ++ show (fmap length repos) ++ " Repositories"
  prs <- getRepoPullRequests token "domain-group" "fe-ops-agency-server"
  print prs

