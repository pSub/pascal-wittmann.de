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
            runDB $ update $ \user -> do
                    set user [UserName =. val (Just name)]
                    where_ (user ^. UserId ==. val uid)
            redirect ProfileR
         _ -> return ()
    defaultLayout $ do
       setTitle "Profile"
       $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = getProfileR
