{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Env
import Lib

data Opts = Opts { token :: String
                 , user :: String
                 , search :: String
                 }

optsParse :: IO Opts
optsParse = Env.parse (header "prbot") $ Opts <$> var (str <=< nonempty) "GITHUB_TOKEN"  (help "github oauth2token" <> keep)
                                              <*> var (str <=< nonempty) "GITHUB_USER"   (help "github user or organisation to scope repos to" <> keep)
                                              <*> var (str <=< nonempty) "GITHUB_SEARCH" (help "search string to limit prs to resultset" <> keep)

main :: IO ()
main = do
  Opts { token
       , user
       , search
       } <- optsParse

  repos <- getUserRepos token user search
  print $ "Finding PRs for " ++ show (fmap length repos) ++ " Repositories"
  prs <- getRepoPullRequests token user "fe-ops-agency-server"
  print prs

