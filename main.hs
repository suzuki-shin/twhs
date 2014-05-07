{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Common
import qualified TwhsConfig as Config (oauthConsumerKey, oauthConsumerSecret)

main :: IO ()
main = do
    aTokenExists <- isJust <$> lookupEnv "OAUTH_ACCESS_TOKEN"
    aSecretExists <- isJust <$> lookupEnv "OAUTH_ACCESS_SECRET"
    if aTokenExists && aSecretExists
        then action
        else oauthPin

action :: IO ()
action = do
    args <- getArgs
    if null args
        then timeline
        else post

post :: IO ()
post = runTwitterFromEnv' $ do
    status <- T.concat . map T.pack <$> liftIO getArgs
    liftIO $ T.putStrLn $ status <> "[y/N]"
    ans <- liftIO getLine
    if ans == "y"
        then do
            liftIO $ T.putStrLn $ "Post message: " <> status
            res <- call $ update status
            liftIO $ print res
        else liftIO $ T.putStrLn "canceled."

timeline :: IO ()
timeline = runTwitterFromEnv' $ do
    liftIO . putStrLn $ "# your home timeline (up to 100 tweets):"
    sourceWithMaxId homeTimeline
        C.$= CL.isolate 100
        C.$$ CL.mapM_ $ \status -> liftIO $ do
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
    return $
        twitterOAuth
        { oauthConsumerKey = S8.pack consumerKey
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
