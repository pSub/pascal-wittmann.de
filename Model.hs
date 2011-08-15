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

Tag
  name String

Category
  name String Asc
  CategoryUniq name
  
Article
  title Text Eq Update
  content Markdown Update
  cat CategoryId Update Eq
  meta_descr String
  date UTCTime Desc
|]
