{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Env
import Lib
import Data.Aeson
import qualified Data.Text as T

data Opts = Opts { token :: String
                 , user :: String
                 , search :: String
                 }

optsParse :: IO Opts
optsParse = Env.parse (header "prbot") $ Opts <$> var (str <=< nonempty) "GITHUB_TOKEN"  (help "github oauth2token" <> keep)
                                              <*> var (str <=< nonempty) "GITHUB_USER"   (help "github user or organisation to scope repos to" <> keep)
                                              <*> var (str <=< nonempty) "GITHUB_SEARCH" (help "search string to limit prs to resultset" <> keep)

extractRepos Nothing = []
extractRepos (Just []) = []
extractRepos (Just ((String x):xs)) = (T.unpack x) : extractRepos (Just xs)

main :: IO ()
main = do
  Opts { token
       , user
       , search
       } <- optsParse

  repos <- getUserRepos token user search
  print $ "Finding PRs for " ++ show (fmap length repos) ++ " Repositories"
  prs <- sequence (fmap (getRepoPullRequests token user) (extractRepos (fmap (take 10) repos)))
  print prs

