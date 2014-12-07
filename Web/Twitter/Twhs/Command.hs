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
  , streaming
  , showHelp
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import System.Console.ANSI
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Network.HTTP.Conduit
import Network.HTTP.Conduit as HTTP
import qualified Data.Conduit.Binary as CB

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
listStatuses listName_ = timeline (listsStatuses (ListNameParam listName_))


timeline :: forall apiName. HasMaxIdParam (APIRequest apiName [Status]) => APIRequest apiName [Status] -> Int -> IO ()
timeline timeline_ n = do
  twInfo <- getTWInfoFromEnv
  withManager $ \mgr -> do
    sourceWithMaxId twInfo mgr timeline_
      C.$= CL.isolate n
      C.$$ CL.mapM_ $ \status -> liftIO $ do
        putColorStr Vivid Blue Dull Black $ show $ status ^. statusId
        putColorStr Vivid White Dull Black ": "
        putColorStr Vivid Yellow Dull Black $ T.unpack $ status ^. statusUser . userScreenName
        putColorStr Vivid White Dull Black ": "
        putColorStr Vivid White Dull Black $ T.unpack $ status ^. statusText
        putStrLn ""


showHelp :: IO ()
showHelp = putStr usage


putColorStr :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
putColorStr fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []


streaming :: IO ()
streaming = do
  twInfo <- getTWInfoFromEnv
  withManager $ \mgr -> do
    src <- stream twInfo mgr userstream
    src C.$$+- CL.mapM_ (^! act (liftIO . printTL))


printTL :: StreamingAPI -> IO ()
printTL (SStatus status) = liftIO $ do
  putColorStr Vivid Blue Dull Black $ show $ status ^. statusId
  putColorStr Vivid White Dull Black ": "
  putColorStr Vivid Yellow Dull Black $ T.unpack $ status ^. statusUser . userScreenName
  putColorStr Vivid White Dull Black ": "
  putColorStr Vivid White Dull Black $ T.unpack $ status ^. statusText
  putStrLn ""
printTL (SRetweetedStatus status) = liftIO $ do
  putColorStr Vivid Blue Dull Black $ show $ status ^. rsId
  putColorStr Vivid White Dull Black ": "
  putColorStr Vivid Yellow Dull Black $ T.unpack $ status ^. user . userScreenName
  putColorStr Vivid White Dull Black ": "
  putColorStr Vivid Black Vivid Green "RT"
  putColorStr Vivid White Dull Black " @"
  putColorStr Vivid White Dull Black $ T.unpack $ showStatus $ status ^. rsRetweetedStatus
  putStrLn ""
printTL (SEvent event)
    | (event^.evEvent) == "favorite" || (event^.evEvent) == "unfavorite",
      Just (ETStatus st) <- (event ^. evTargetObject) = do
          let (fromUser, fromIcon) = evUserInfo (event^.evSource)
              (toUser, _toIcon) = evUserInfo (event^.evTarget)
              evUserInfo (ETUser u) = (u ^. userScreenName, u ^. userProfileImageURL)
              evUserInfo _ = ("", Nothing)
              header = T.concat [ event ^. evEvent, "[", fromUser, " -> ", toUser, "]"]
          T.putStrLn $ T.concat [ header, " :: ", showStatus st ]
          icon <- case fromIcon of
              Just iconUrl -> Just <$> fetchIcon (T.unpack fromUser) (T.unpack iconUrl)
              Nothing -> return Nothing
          notifySend header (showStatus st) icon
printTL s = print s

showStatus :: AsStatus s => s -> T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text
                        ]


notifySend :: T.Text -> T.Text -> Maybe FilePath -> IO ()
notifySend header content icon = do
    let ic = maybe [] (\i -> ["-i", i]) icon
    void $ rawSystem "notify-send" $ [T.unpack header, T.unpack content] ++ ic

fetchIcon :: String -- ^ screen name
          -> String -- ^ icon url
          -> IO String
fetchIcon sn url = do
    ipath <- iconPath
    let fname = ipath </> sn ++ "__" ++ takeFileName url
    exists <- doesFileExist fname
    unless exists $ withManager $ \mgr -> do
        req <- liftIO $ parseUrl url
        body <- http req mgr
        HTTP.responseBody body C.$$+- CB.sinkFile fname
    return fname

iconPath :: IO FilePath
iconPath = (</> "icons") <$> confdir >>= ensureDirectoryExist

ensureDirectoryExist :: FilePath -> IO FilePath
ensureDirectoryExist dir = do
    createDirectoryIfMissing True dir
    return dir

confdir :: IO FilePath
confdir = fmap (</> ".twitter-conduit") getHomeDirectory >>= ensureDirectoryExist
