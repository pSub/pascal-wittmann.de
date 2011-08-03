{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}

module Model where

import Yesod
import Yesod.Goodies.Markdown
import Database.Persist.Base

import Data.Time
import Data.Text (Text)

share2 mkPersist (mkMigrate "migrateAll") [persist|
User
  ident Text
  password Text Maybe Update
  UniqueUser ident

Email
  email Text
  user UserId Maybe Update
  verkey Text Maybe Update
  UniqueEmail email

Module
  name String

Article
  title Text Eq Update
  content Markdown Update
  meta_descr String
  date UTCTime Desc
  
Tag
  name String
|]
