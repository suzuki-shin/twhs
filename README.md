# twhs

[![Build Status](https://travis-ci.org/suzuki-shin/twhs.svg?branch=master)](https://travis-ci.org/suzuki-shin/twhs)

## About ##

CLI twitter client. Inspired by Tw (http://shokai.github.io/tw/)

![スクリーンショット](https://dl.dropboxusercontent.com/u/2544479/%E3%82%B9%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3%E3%82%B7%E3%83%A7%E3%83%83%E3%83%88%202014-06-02%2013.44.20.png)

## Install ##
$ cabal update && cabal install twhs

## Example ##

```
# tweet
$ twhs Lunch now!
```
```
# get timeline only 50 lines
$ twhs -n 50
```
```
# get user timeline
$ twhs -u shin16s
```
```
# in reply to
$ twhs -r 123456 ok! let's go
```

## Options ##

```
  -r ID MESSAGE  --reply=ID MESSAGE  in reply to ID
  -R ID          --retweet=ID        retweet ID
  -u USER        --user=USER         show user timeline
  -m             --mention           show mention timeline
  -f ID          --fav=ID            fav to ID
  -n NUM         --num=NUM           take NUM
  -h             --help              show help
```
