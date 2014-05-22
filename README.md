# twhs

[![Build Status](https://travis-ci.org/suzuki-shin/twhs.svg?branch=master)](https://travis-ci.org/suzuki-shin/twhs)

## About ##

This is cli twitter client.

## Usage ##

```
  -r ID MESSAGE  --reply=ID MESSAGE  in reply to ID
  -R ID          --retweet=ID        retweet ID
  -u USER        --user=USER         show user timeline
  -m             --mention           show mention timeline
  -f ID          --fav=ID            fav to ID
  -n NUM         --num=NUM           take NUM
  -h             --help              show help
```

## Example ##

```
# tweet
$ twhs Lunch now!
```
```
# get timeline
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
