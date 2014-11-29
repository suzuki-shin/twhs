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
  , listStatuses
  , showHelp
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import System.Console.ANSI
import Network.HTTP.Conduit

import Web.Twitter.Twhs.Common
import Web.Twitter.Twhs.Option

type TweetID = Integer
type UserScreenName = String

tweet :: T.Text -> IO ()
tweet status = post_ $ update status

reply :: TweetID -> T.Text -> IO ()
reply replyTo status = post_ (update status & inReplyToStatusId ?~ replyTo)

retweet :: TweetID -> IO ()
retweet rtId = do
  twInfo <- getTWInfoFromEnv
  liftIO $ T.putStrLn $ "retweet " <> (T.pack . show) rtId <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      withManager $ \mgr -> do
        res <- call twInfo mgr $ retweetId rtId
        liftIO $ print res
    else liftIO $ T.putStrLn "canceled."

fav :: TweetID -> IO ()
fav favTo = do
  twInfo <- getTWInfoFromEnv
  liftIO $ T.putStrLn $ "fav to " <> (T.pack . show) favTo <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      withManager $ \mgr -> do
        res <- call twInfo mgr $ favoritesCreate favTo
        liftIO $ print res
    else liftIO $ T.putStrLn "canceled."

-- post_ :: forall (m :: * -> *) apiName a.
--          (Show a, MonadBaseControl IO m, MonadIO m, C.MonadUnsafeIO m,
--           C.MonadThrow m, Data.Aeson.Types.Class.FromJSON a) =>
--          APIRequest apiName a -> m ()
post_ update_status = do
  status <- T.intercalate " " . Prelude.map T.pack <$> liftIO getArgs
  liftIO $ T.putStrLn $ status <> "[y/N]"
  ans <- liftIO getLine
  if ans == "y"
    then do
      T.putStrLn $ "Post message: " <> status
      twInfo <- getTWInfoFromEnv
      res <- withManager $ \mgr -> call twInfo mgr update_status
      print res
    else liftIO $ T.putStrLn "canceled."


homeTL :: Int -> IO ()
homeTL = timeline homeTimeline

mentionTL :: Int -> IO ()
mentionTL = timeline mentionsTimeline

userTL :: UserScreenName -> Int -> IO ()
userTL uName = timeline (userTimeline (ScreenNameParam uName))


listStatuses :: String -> Int -> IO ()
listStatuses listName = timeline (listsStatuses (ListNameParam listName))


-- timeline :: forall (m :: * -> *) apiName.
--             (HasMaxIdParam (APIRequest apiName [Status]),
--              MonadBaseControl IO m, MonadIO m, C.MonadUnsafeIO m,
--              C.MonadThrow m) =>
--             APIRequest apiName [Status] -> Int -> m ()
timeline timeline_ n = do
  twInfo <- getTWInfoFromEnv
  withManager $ \mgr -> do
    sourceWithMaxId twInfo mgr timeline_
      C.$= CL.isolate n
      C.$$ CL.mapM_ $ \status -> liftIO $ do
        colorStr Vivid Blue Dull Black $ show $ status ^. statusId
        colorStr Vivid White Dull Black ": "
        colorStr Vivid Yellow Dull Black $ T.unpack $ status ^. statusUser . userScreenName
        colorStr Vivid White Dull Black ": "
        colorStr Vivid White Dull Black $ T.unpack $ status ^. statusText
        putStrLn ""


showHelp :: IO ()
showHelp = putStr usage


colorStr :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStr fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
