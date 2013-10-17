{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Yesod.Markdown (Markdown)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Typeable
import Database.Persist.Quasi


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll", mkDeleteCascade sqlOnlySettings]
      $(persistFileWith lowerCaseSettings "config/models")


