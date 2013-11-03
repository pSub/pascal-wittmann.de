{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
--    , runDB
    , Spec
    , Example
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist       hiding (get)
import           Database.Persist.Sql   (SqlPersistM, runSqlPersistMPool)
import           Yesod.Test

import           Foundation
import           Model

type Spec = YesodSpec App
type Example = YesodExample App

-- TODO: Write some meaningful test
-- runDB :: SqlPersistM a -> Example a
-- runDB query = do
--     pool <- fmap connPool getTestYesod
--     liftIO $ runSqlPersistMPool query pool
