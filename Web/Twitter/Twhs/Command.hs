{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Web.Twitter.Twhs.Command (
    tweet
  , reply
  , retweet
  , fav
  , homeTL
  , mentionTL
  , userTL
  , showHelp
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Web.Twitter.Conduit
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Monoid
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Web.Twitter.Twhs.Common
import Web.Twitter.Twhs.Option

type TweetID = Integer
type UserScreenName = String

tweet :: T.Text -> IO ()
tweet status = post_ $ update status

reply :: TweetID -> T.Text -> IO ()
reply replyTo status = post_ (update status & inReplyToStatusId ?~ replyTo)

retweet :: TweetID -> IO ()
retweet rtId = runTwitterFromEnv' $ do
  liftIO $ T.putStrLn $ "retweet " <> (T.pack . show) rtId <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      res <- call $ retweetId rtId
      liftIO $ print res
    else liftIO $ T.putStrLn "canceled."

fav :: TweetID -> IO ()
fav favTo = runTwitterFromEnv' $ do
  liftIO $ T.putStrLn $ "fav to " <> (T.pack . show) favTo <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      res <- call $ favoritesCreate favTo
      liftIO $ print res
    else liftIO $ T.putStrLn "canceled."

-- post_ :: forall (m :: * -> *) apiName a.
--          (Show a, MonadBaseControl IO m, MonadIO m, C.MonadUnsafeIO m,
--           C.MonadThrow m, Data.Aeson.Types.Class.FromJSON a) =>
--          APIRequest apiName a -> m ()
post_ update_status = runTwitterFromEnv' $ do
  status <- T.intercalate " " . map T.pack <$> liftIO getArgs
  liftIO $ T.putStrLn $ status <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      liftIO $ T.putStrLn $ "Post message: " <> status
      res <- call update_status
      liftIO $ print res
    else liftIO $ T.putStrLn "canceled."


homeTL :: Int -> IO ()
homeTL = timeline homeTimeline

mentionTL :: Int -> IO ()
mentionTL = timeline mentionsTimeline

userTL :: UserScreenName -> Int -> IO ()
userTL uName = timeline (userTimeline (ScreenNameParam uName))

timeline :: forall (m :: * -> *) apiName.
            (HasMaxIdParam (APIRequest apiName [Status]),
             MonadBaseControl IO m, MonadIO m, C.MonadUnsafeIO m,
             C.MonadThrow m) =>
            APIRequest apiName [Status] -> Int -> m ()
timeline timeline_ n = runTwitterFromEnv' $
  sourceWithMaxId timeline_
    C.$= CL.isolate n
    C.$$ CL.mapM_ $ \status -> liftIO $
      T.putStrLn $ T.concat [ T.pack . show $ status ^. statusId
                            , ": "
                            , status ^. statusUser . userScreenName
                            , ": "
                            , status ^. statusText
                            ]

showHelp :: IO ()
showHelp = putStr usage
