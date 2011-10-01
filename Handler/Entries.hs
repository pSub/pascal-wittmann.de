{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Entries
       ( getEntriesR
       , getEntriesByTagR
       , getEntryR
       , postEntryR
       , getEntryCommentR
       , postEntryCommentR
       , getNewEntryR
       , getDeleteTagR
       , getDeleteEntryR
       , postNewEntryR
       , getEditEntryR
       , postEditEntryR
       ) where

import Foundation
import Yesod.Form
import Yesod.Goodies.Markdown
import Database.Persist.Join (selectOneMany, SelectOneMany(..))
import Database.Persist.Join.Sql (runJoin)

import Prelude hiding (unwords)
import Control.Applicative
import Data.List (find, intersperse, unwords)
import Data.Time
import Data.Text (Text, unpack, pack, append, strip)
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Tree

data Params = Params
     { title :: Text
     , ident :: Text
     , cat :: CategoryId
     , tag :: Text
     , recap :: Text
     , text :: Markdown
     } deriving Show
                
data PComment = PComment
     { parent :: Maybe CommentId
     , author :: Maybe Text
     , content :: Markdown
     }
     
paramsFormlet ::  Maybe Params -> [(Text, CategoryId)] -> Html -> Form Homepage Homepage (FormResult Params, Widget)
paramsFormlet mparams cats html = (flip renderDivs) html $ Params
    <$> areq textField "Title" (title <$> mparams)
    <*> areq textField "Ident" (ident <$> mparams)
    <*> areq (selectField cats) "Kategorie" (cat <$> mparams)
    <*> areq textField "Tags" (tag <$> mparams)
    <*> areq textField "Summary" (recap <$> mparams)
    <*> areq markdownField "Text" (text <$> mparams)

commentForm :: Maybe PComment -> Maybe CommentId -> Html -> Form Homepage Homepage (FormResult PComment, Widget)
commentForm mcomment mparent html = (flip renderDivs) html $ PComment
    <$> pure mparent
    <*> aopt textField "Name" (author <$> mcomment)
    <*> areq markdownField "Kommentar" (content <$> mcomment)

getEntriesR :: Text -> Handler RepHtml
getEntriesR catName = do
  mu <- maybeAuth
  mcat <- runDB $ getBy $ CategoryUniq catName
  cat <- mcat -|- notFound
  tagsEntries <- runDB $ runJoin (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterOne = [TagCategory ==. (fst cat)]
          , somOrderOne = [Asc TagName]
          }
  tags <- return $ map fst tagsEntries
  entries <- runDB $ selectList [EntryCat ==. (fst cat)] [Desc EntryDate]
  defaultLayout $ do
    setTitle $ toHtml catName
    addCassius $(cassiusFile "entries")
    addWidget $(widgetFile "entries")

getEntriesByTagR :: Text -> Text -> Handler RepHtml
getEntriesByTagR catName tagName' = do
  mu <- maybeAuth
  mcat <- runDB $ getBy $ CategoryUniq catName
  cat <- mcat -|- notFound
  mtag <- runDB $ getBy $ UniqueTag tagName' (fst cat)
  tag <- mtag -|- notFound
  
  tagsEntries <- runDB $ runJoin (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterOne = [TagCategory ==. (fst cat)]
          , somOrderOne = [Asc TagName]
          }
  tags <- return $ map fst tagsEntries
  entries <- runDB $ runJoin (selectOneMany (TaggedEntry <-.) taggedEntry)
             { somFilterMany = [TaggedTag ==. (fst tag)]
             , somOrderOne = [Desc EntryDate]
             }
  entries <- return $ map fst entries
  defaultLayout $ do
    setTitle $ toHtml $ catName `append` " :: " `append` tagName'
    addCassius $(cassiusFile "entries")
    addWidget $(widgetFile "entries")

entryHandler :: Text -> Text -> Maybe CommentId -> Handler RepHtml
entryHandler catName ident mparent = do
  mu <- maybeAuth
  mentry <- runDB $ getBy $ EntryUniq ident
  entry <- mentry -|- notFound
  tags <- runDB $ runJoin $ (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterMany = [TaggedEntry ==. (fst entry)] 
          , somOrderOne = [Asc TagName]
          }
  ucomments <- runDB $ selectList [CommentEntry ==. (fst entry)] [Asc CommentDate]
  comments <- return $ buildComments ucomments
  ((res, form), enctype) <- runFormPost $ commentForm Nothing mparent
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      _ <- runDB $ insert $ Comment (author p) (content p) now (parent p) (fst entry)
      redirect RedirectTemporary $ EntryR catName ident
    _ -> return ()
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle $ snd entry
    addCassius $(cassiusFile "entry")
    addWidget $(widgetFile "entry")
    
getEntryCommentR :: Text -> Text -> CommentId -> Handler RepHtml
getEntryCommentR catName ident parent = entryHandler catName ident (Just parent)
    
postEntryCommentR :: Text -> Text -> CommentId -> Handler RepHtml
postEntryCommentR = getEntryCommentR

getEntryR :: Text -> Text ->  Handler RepHtml
getEntryR catName ident = entryHandler catName ident Nothing

postEntryR :: Text -> Text -> Handler RepHtml
postEntryR = getEntryR
    
getDeleteTagR :: Text -> TagId -> Handler ()
getDeleteTagR category tid = do
  _ <- requireAuth
  runDB $ delete tid
  redirect RedirectTemporary $ EntriesR category

getNewEntryR :: Handler RepHtml
getNewEntryR = do
  _ <- requireAuth
  catOpt <- categories
  tags <- return []
  ((res, form), enctype) <- runFormPost $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      aid <- runDB $ insert $ Entry (title p) (ident p) (text p) (cat p) (recap p) "" now
      insertTags (cat p) aid $ splitOn "," $ filter (/= ' ') (unpack $ tag p)
      redirect RedirectTemporary $ EntriesR $ category (cat p) catOpt
    _ -> return ()
  defaultLayout $ do
    setTitle "New Entry"
    addWidget $(widgetFile "new-entry")

postNewEntryR :: Handler RepHtml
postNewEntryR = getNewEntryR

getEditEntryR :: EntryId -> Handler RepHtml
getEditEntryR eid = do
  _ <- requireAuth
  ma <- runDB $ get eid
  catOpt <- categories
  tags' <- runDB $ runJoin $ (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterMany = [TaggedEntry ==. eid]
          , somOrderOne = [Asc TagName]
          }
  tags <- return $ pack $ concat $ intersperse ", " $ map (unpack . tagName . snd . fst) tags'
  case ma of
    Just a ->
      do
        ((res, form), enctype) <- runFormGet $ paramsFormlet (Just $ Params (entryTitle a) (entryIdent a) (entryCat a) tags (entryRecap a) (entryContent a)) catOpt
        case res of
          FormSuccess p -> do
            runDB $ update eid [EntryTitle =. (title p), EntryIdent =. (ident p), EntryCat =. (cat p), EntryContent =. (text p)]
            runDB $ deleteWhere [TaggedEntry ==. eid]
            insertTags (cat p) eid $ splitOn "," $ filter (/= ' ') (unpack $ strip $ tag p)
            redirect RedirectTemporary $ EntriesR $ category (cat p) catOpt
          _ -> redirect RedirectTemporary (EditEntryR eid)
        defaultLayout $ do
           setTitle "Edit Entry"
           addWidget $(widgetFile "new-entry")
    Nothing -> do
      redirect RedirectTemporary NewEntryR
    
postEditEntryR :: EntryId -> Handler RepHtml
postEditEntryR = getEditEntryR
      
getDeleteEntryR :: Text -> EntryId -> Handler ()
getDeleteEntryR category eid = do
  _ <- requireAuth
  runDB $ deleteWhere [CommentEntry ==. eid]
  runDB $ deleteWhere [TaggedEntry ==. eid]
  runDB $ delete eid
  redirect RedirectTemporary $ EntriesR category

-- Helper functions
categories = do
  cas <- runDB $ selectList [] [Asc CategoryName]
  return $ map (\ c -> (categoryName $ snd c, fst c)) cas

category c = fst . fromJust . find ((== c) . snd)

tagsForEntry eid = map fst . filter (any ((== eid) . taggedEntry . snd) . snd)

insertTags :: CategoryId -> EntryId -> [String] -> Handler ()
insertTags category eid (t:tags) = do
  mtag <- runDB $ getBy $ UniqueTag (pack t) category
  tid <- if isJust mtag then
           return $ fst $ fromJust mtag
         else
           runDB $ insert $ Tag (pack t) category
  _ <- runDB $ insert $ Tagged tid eid
  insertTags category eid tags
insertTags _ _ [] = return ()

buildComments cs = concat $ map flatten $ unfoldForest (\ c -> (c, getChilds c)) roots
      where
        roots = zip [1,1..] (filter (isNothing . commentParent . snd) cs)
        getChilds c = filter (isChild (snd c) . snd) (zip (repeat $ 1 + (fst c)) cs)
        isChild c c' = (isJust $ getParent c') && (fst c) == (fromJust $ getParent c')
        getParent = commentParent . snd

(-|-) :: Monad m => Maybe a -> m a -> m a
Just a -|- _ = return a
Nothing -|- action = do action
