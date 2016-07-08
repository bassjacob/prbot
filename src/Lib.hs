{-# LANGUAGE OverloadedStrings #-}

module Lib ( someFunc
           ) where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified System.Environment as E (lookupEnv, getArgs)
import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI

apiUrl :: [String] -> Maybe String
apiUrl [] = Nothing
apiUrl [_] = Nothing
apiUrl (name:user:_) = Just $ "https://api.github.com/search/repositories?q="
                            ++ name
                            ++ "+user:"
                            ++ user

getRepos :: Maybe String -> Maybe String -> IO (Maybe [Value])
getRepos _ Nothing = return Nothing
getRepos Nothing _ = return $ Just []
getRepos (Just url) (Just token) = do
  response <- getWith opts url
  let next = response ^? responseLink "rel" "next" . linkURL
  nextRepos <- getRepos (B.unpack <$> next) (Just token)
  let repos = response
              ^.. responseBody
              . key "items"
              . _Array
              . traverse
              . key "name"
  return $ liftM2 (++) (Just repos) nextRepos
    where
      opts = defaults & auth ?~ oauth2Token (B.pack token)

someFunc :: IO ()
someFunc = do
  token <- E.lookupEnv "GITHUB_TOKEN"
  args <- E.getArgs
  repos <- getRepos (apiUrl args) token
  print repos
