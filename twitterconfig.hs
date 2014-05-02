{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TwitterConfig where

import Web.Twitter.Conduit
import Web.Authenticate.OAuth (OAuth(..), Credential(..))

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "UoNgjgh43ukUCkKBtCAFNYg96"
    , oauthConsumerSecret = "IEuzxBR7ykDtB9iEy7UkipQyzKmtLksd5gMSTzmsYXOgQsl5XD"
    }
