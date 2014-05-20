{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Web.Twitter.Twhs

type TweetID = Integer
type UserScreenName = String

main :: IO ()
main = do
  aTokenExists <- isJust <$> lookupEnv "OAUTH_ACCESS_TOKEN"
  aSecretExists <- isJust <$> lookupEnv "OAUTH_ACCESS_SECRET"
  if aTokenExists && aSecretExists
    then action
    else oauthPin
