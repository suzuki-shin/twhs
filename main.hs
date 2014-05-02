{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import System.Environment

import Control.Lens
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Common

main :: IO ()
main = do
    args <- getArgs
    print args
    if null args
        then timeline
        else post

post :: IO ()
post = runTwitterFromEnv' $ do
    status <- T.concat . map T.pack <$> liftIO getArgs
    liftIO $ T.putStrLn $ "Post message: " <> status
    res <- call $ update status
    liftIO $ print res

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
