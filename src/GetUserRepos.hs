{-# LANGUAGE OverloadedStrings #-}

module GetUserRepos ( getUserRepos
                    ) where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI

githubAuthOpts :: B.ByteString -> Options
githubAuthOpts token = defaults & auth ?~ oauth2Token token

apiUrl :: String -> String -> Maybe String
apiUrl search user =
  Just $ "https://api.github.com/search/repositories?q=" ++ search
                                                         ++ "+user:"
                                                         ++ user

extractRepos :: Response BLI.ByteString -> [Value]
extractRepos = flip (^..) $ responseBody . key "items"
                                         . _Array
                                         . traverse
                                         . key "name"

getPages :: Maybe String -> B.ByteString -> IO (Maybe [Value])
getPages Nothing _ = return $ Just []
getPages (Just url) token = do
  response <- (getWith . githubAuthOpts) token url

  let repos = extractRepos response

  let next = B.unpack <$> response ^? responseLink "rel" "next" . linkURL
  nextRepos <- getPages next token

  return $ liftM2 (++) (Just repos) nextRepos

getUserRepos :: Maybe String -> [String] -> IO (Maybe [Value])
getUserRepos Nothing _ = return Nothing
getUserRepos _ [] = return Nothing
getUserRepos _ [_] = return Nothing
getUserRepos (Just token) (search:user:_) = getPages (apiUrl search user) (B.pack token)
