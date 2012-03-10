module Handler.Profile
       ( getProfileR
       , postProfileR
       ) where

import Import

data Profile = Profile
             { name :: Maybe Text }

profileForm :: Maybe Profile -> Html -> MForm Homepage Homepage (FormResult Profile, Widget)
profileForm profile html = (flip renderDivs) html $ Profile
     <$> aopt textField "Name" (name <$> profile)

getProfileR :: Handler RepHtml
getProfileR = do
    Entity uid u <- requireAuth
    ((res, form), enctype) <- runFormPost $ profileForm $ Just (Profile $ userName u)
    case res of
         FormSuccess p -> do
            runDB $ update uid [UserName =. (name p)]
            redirect ProfileR
         _ -> return ()
    defaultLayout $ do
       setTitle "Profil"
       $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR
