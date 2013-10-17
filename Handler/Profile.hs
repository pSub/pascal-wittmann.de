module Handler.Profile
       ( getProfileR
       , postProfileR
       ) where

import           Import

profileForm :: Maybe Text -> Form Text
profileForm name = renderDivs $ areq textField (fieldSettingsLabel MsgName) name

getProfileR :: Handler Html
getProfileR = do
    Entity uid u <- requireAuth
    ((res, form), enctype) <- runFormPost $ profileForm $ userName u
    case res of
         FormSuccess name -> do
            runDB $ update uid [UserName =. (Just name)]
            redirect ProfileR
         _ -> return ()
    defaultLayout $ do
       setTitle "Profile"
       $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = getProfileR
