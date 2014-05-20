{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Monoid
import Network.HTTP.Conduit
import System.Environment
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.GetOpt

import Common
import qualified TwhsConfig as Config (oauthConsumerKey, oauthConsumerSecret)

type TweetID = Integer
type UserScreenName = String

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
      Nothing -> case optRetweet opt of
        Just rtId -> retweet rtId
        Nothing -> case optFavTo opt of
          Just favTo -> fav favTo
          Nothing -> if null mess
                     then homeTL num
                     else tweet status

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


getTokens :: IO OAuth
getTokens = do
  let consumerKey = Config.oauthConsumerKey
      consumerSecret = Config.oauthConsumerSecret
  return $ twitterOAuth {
      oauthConsumerKey = S8.pack consumerKey
    , oauthConsumerSecret = S8.pack consumerSecret
    , oauthCallback = Just "oob"
    }

authorize :: (MonadBaseControl IO m, C.MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> Manager
          -> m Credential
authorize oauth mgr = do
  cred <- OA.getTemporaryCredential oauth mgr
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  where
    getPIN url = liftIO $ do
      putStrLn $ "browse URL: " ++ url
      putStr "> what was the PIN twitter provided you with? "
      hFlush stdout
      S8.getLine

oauthPin :: IO ()
oauthPin = do
  tokens <- getTokens
  Credential cred <- liftIO $ withManager $ authorize tokens
  print cred

  S8.putStrLn . S8.intercalate "\n" $
    [ "export OAUTH_ACCESS_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" cred) <> "\""
    , "export OAUTH_ACCESS_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" cred) <> "\""
    ]

data Options = Options {
   optReplyTo      :: Maybe Integer
 , optRetweet      :: Maybe Integer
 , optFavTo        :: Maybe Integer
 , optUserTimeLine :: Maybe String
 , optNum          :: Int
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options {
   optReplyTo      = Nothing
 , optRetweet      = Nothing
 , optFavTo        = Nothing
 , optUserTimeLine = Nothing
 , optNum          = 30
 }

options :: [OptDescr (Options -> Options)]
options = [
   Option "r" ["reply"]   (ReqArg (\sid opts -> opts { optReplyTo = Just (read sid) }) "ID MESSAGE") "in reply to ID"
 , Option "R" ["retweet"] (ReqArg (\sid opts -> opts { optRetweet = Just (read sid) }) "ID") "retweet ID"
 , Option "u" ["user"]    (ReqArg (\u opts -> opts { optUserTimeLine = Just u }) "USER") "show user timeline"
 , Option "f" ["fav"]     (ReqArg (\sid opts -> opts { optFavTo = Just (read sid) }) "ID") "fav to ID"
 , Option "n" ["num"]   (ReqArg (\num opts -> opts { optNum = read num }) "NUM") "take NUM"
 ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv = case getOpt Permute options argv of
  (o,n,[] ) -> return (foldl (flip id) defaultOptions o, n)
  (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: twhs [OPTION...] [STATUS..]"
