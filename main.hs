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
-- import Web.Authenticate.OAuth (OAuth(..), Credential(..))
-- import qualified Web.Authenticate.OAuth as OA
-- import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import qualified Data.ByteString.Char8 as B8
-- import Data.Default
-- import Control.Monad.Logger
-- import Control.Monad.Trans.Control
-- import Control.Monad.Trans.Resource
-- import Control.Monad.IO.Class
-- import System.IO (hFlush, stdout)


import Common

main :: IO ()
main = do
    args <- getArgs
    print args
    if null args
        then timeline
--         then putStr "hoge"
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
