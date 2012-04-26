module Handler.Profile
       ( getProfileR
       , postProfileR
       ) where

import Import

profileForm :: Maybe Text -> Form Text
profileForm name = renderDivs $ areq textField "Name" name

getProfileR :: Handler RepHtml
getProfileR = do
    Entity uid u <- requireAuth
    ((res, form), enctype) <- runFormPost $ profileForm $ userName u
    case res of
         FormSuccess name -> do
            runDB $ update uid [UserName =. (Just name)]
            redirect ProfileR
         _ -> return ()
    defaultLayout $ do
       setTitle "Profil"
       $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR
