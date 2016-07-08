{-# LANGUAGE OverloadedStrings #-}

module Lib ( someFunc
           ) where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified System.Environment as E (lookupEnv)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI

apiURL = "https://api.github.com/search/repositories?q=.dotfiles+user:subshad"

getRepos :: Maybe String -> IO (Maybe [Value])
getRepos Nothing = return Nothing
getRepos (Just token) = do
  response <- getWith opts apiURL
  let repos = response
              ^.. responseBody
              . key "items"
              . _Array
              . traverse
              . key "name"
  return $ Just repos
    where
      opts = defaults & auth ?~ oauth2Token (B.pack token)

someFunc :: IO ()
someFunc = do
  token <- E.lookupEnv "GITHUB_TOKEN"
  status <- getRepos token
  print status
