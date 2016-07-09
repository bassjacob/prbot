{-# LANGUAGE OverloadedStrings #-}

module Lib ( someFunc
           ) where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI

apiUrl :: String -> String -> String
apiUrl search user =
  "https://api.github.com/search/repositories?q="
    ++ search
    ++ "+user:"
    ++ user

getPage url token = getWith opts url
  where opts = defaults & auth ?~ oauth2Token (B.pack token)

extractRepos = flip (^..) $ responseBody
                              . key "items"
                              . _Array
                              . traverse
                              . key "name"

getRepos :: Maybe String -> String -> IO (Maybe [Value])
getRepos Nothing _ = return $ Just []
getRepos (Just url) token = do
  response <- getPage url token
  let repos = extractRepos response
  let next = B.unpack <$> response ^? responseLink "rel" "next" . linkURL
  nextRepos <- getRepos next token
  return $ liftM2 (++) (Just repos) nextRepos

someFunc :: Maybe String -> [String] -> IO (Maybe [Value])
someFunc Nothing _ = return Nothing
someFunc _ [] = return Nothing
someFunc _ [_] = return Nothing
someFunc (Just token) (search:user:_) = getRepos (Just (apiUrl search user)) token
