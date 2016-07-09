{-# LANGUAGE OverloadedStrings #-}

module GetRepoPullRequests ( getRepoPullRequests
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
apiUrl owner repo = Just $ "https://api.github.com/repos/" ++ owner
                                                           ++ "/"
                                                           ++ repo
                                                           ++ "/pulls"

extractPrs :: Response BLI.ByteString -> [Value]
extractPrs = flip (^..) $ responseBody . _Array
                                       . traverse
                                       . key "title"

getPrs Nothing _ = return $ Just []
getPrs (Just url) token = do
  response <- (getWith . githubAuthOpts) token url

  let repos = extractPrs response

  let next = B.unpack <$> response ^? responseLink "rel" "next" . linkURL
  nextRepos <- getPrs next token

  return $ liftM2 (++) (Just repos) nextRepos

getRepoPullRequests Nothing _ _ = return Nothing
getRepoPullRequests (Just token) owner repo = getPrs (apiUrl owner repo) (B.pack token)
