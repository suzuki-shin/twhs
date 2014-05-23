{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Twhs.Common (
    oauthPin
  , runTwitterFromEnv'
  ) where

import Web.Twitter.Conduit

import Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Conduit
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Resource
import System.Environment
import Control.Monad.Logger
import Control.Lens
import Data.Maybe
import Data.Monoid
import System.IO (hFlush, stdout)
import qualified Data.Conduit as C

import qualified Web.Twitter.Twhs.Config as Config (oauthConsumerKey, oauthConsumerSecret)

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let consumerKey = S8.pack Config.oauthConsumerKey
        consumerSecret = S8.pack Config.oauthConsumerSecret
        oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
    env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
    let u = M.lookup "https_proxy" env <|>
            M.lookup "http_proxy" env <|>
            M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
    return $ Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

runTwitterFromEnv :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT m) a -> m a
runTwitterFromEnv task = do
    pr <- liftBase getProxyEnv
    (oa, cred) <- liftBase getOAuthTokens
    let env = (setCredential oa cred def) { twProxy = pr }
    runTW env task

runTwitterFromEnv' :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT (NoLoggingT m)) a -> m a
runTwitterFromEnv' = runNoLoggingT . runTwitterFromEnv


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
