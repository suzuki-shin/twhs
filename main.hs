{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import System.Environment (lookupEnv, getArgs)
import qualified Data.Text as T

import Web.Twitter.Twhs

main :: IO ()
main = do
  aTokenExists <- isJust <$> lookupEnv "OAUTH_ACCESS_TOKEN"
  aSecretExists <- isJust <$> lookupEnv "OAUTH_ACCESS_SECRET"
  if aTokenExists && aSecretExists
    then action
    else oauthPin

action :: IO ()
action = do
  argv <- getArgs
  (opt, mess) <- compilerOpts argv
  let status = (T.intercalate " " . map T.pack) mess
      num = optNum opt
  case optReplyTo opt of
    Just replyTo -> reply replyTo status
    Nothing -> case optUserTimeLine opt of
      Just uname -> userTL uname num
      Nothing -> if optMentionTimeLine opt
        then mentionTL num
        else if optStreamTimeLine opt
          then streaming
          else case optListStatuses opt of
            Just listName -> listStatuses listName num
            Nothing -> case optRetweet opt of
              Just rtId -> retweet rtId
              Nothing -> case optFavTo opt of
                Just favTo -> fav favTo
                Nothing | optHelp opt -> showHelp
                        | null mess -> homeTL num
                        | otherwise -> tweet status

