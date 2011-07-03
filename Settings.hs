{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings
       ( approot
       ) where

import qualified Data.Text as T

approot :: T.Text
approot = "http://localhost:3000"
