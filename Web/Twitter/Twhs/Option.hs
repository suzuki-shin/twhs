module Web.Twitter.Twhs.Option (
    compilerOpts
  , optReplyTo
  , optRetweet
  , optFavTo
  , optUserTimeLine
  , optListStatuses
  , optMentionTimeLine
  , optNum
  , optHelp
  , usage
   ) where

import System.Console.GetOpt
import Paths_twhs (version)
import Data.Version (showVersion)

data Options = Options {
   optReplyTo :: Maybe Integer
 , optRetweet :: Maybe Integer
 , optFavTo :: Maybe Integer
 , optUserTimeLine :: Maybe String
 , optListStatuses :: Maybe String
 , optMentionTimeLine :: Bool
 , optNum :: Int
 , optHelp :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions = Options {
   optReplyTo = Nothing
 , optRetweet = Nothing
 , optFavTo = Nothing
 , optUserTimeLine = Nothing
 , optListStatuses = Nothing
 , optMentionTimeLine = False
 , optNum = 30
 , optHelp = False
 }

options :: [OptDescr (Options -> Options)]
options = [
   Option "r" ["reply"]   (ReqArg (\sid opts -> opts { optReplyTo = Just (read sid) }) "ID MESSAGE") "in reply to ID"
 , Option "R" ["retweet"] (ReqArg (\sid opts -> opts { optRetweet = Just (read sid) }) "ID") "retweet ID"
 , Option "u" ["user"]    (ReqArg (\u opts -> opts { optUserTimeLine = Just u }) "USER") "show user timeline"
 , Option "l" ["list"]    (ReqArg (\l opts -> opts { optListStatuses = Just l }) "LIST") "show list statuses"
 , Option "m" ["mention"] (NoArg (\opts -> opts { optMentionTimeLine = True })) "show mention timeline"
 , Option "f" ["fav"]     (ReqArg (\sid opts -> opts { optFavTo = Just (read sid) }) "ID") "fav to ID"
 , Option "n" ["num"]     (ReqArg (\num opts -> opts { optNum = read num }) "NUM") "take NUM"
 , Option "h" ["help"]    (NoArg (\opts -> opts { optHelp = True })) "show help"
 ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv = case getOpt Permute options argv of
  (o,n,[] ) -> return (foldl (flip id) defaultOptions o, n)
  (_,_,errs) -> ioError (userError (concat errs ++ usage))

usage :: String
usage = usageInfo header options ++ footer
  where
    header = "twhs " ++ showVersion version ++ "\nThis is a command line interface to Twitter.\nUsage: twhs [OPTION...]\n"
    footer = "\n" ++
             "Example:\n\n" ++
             "  -- tweet\n" ++
             "  $ twhs Lunch now!\n\n" ++
             "  -- get timeline\n" ++
             "  $ twhs -n 50\n\n" ++
             "  -- get user timeline\n" ++
             "  $ twhs -u shin16s -n 10\n\n" ++
             "  -- get list statuses\n" ++
             "  $ twhs -l shin16s/haskeller -n 10\n\n" ++
             "  -- in reply to \n" ++
             "  $ twhs -r 123456 ok! let's go\n"

