module Handler.Profile
       ( getProfileR
       , postProfileR
       ) where

import Foundation
import Control.Applicative
import Data.Text (Text)

data Profile = Profile
             { name :: Maybe Text }

profileForm :: Maybe Profile -> Html -> Form Homepage Homepage (FormResult Profile, Widget)
profileForm profile html = (flip renderDivs) html $ Profile
     <$> aopt textField "Name" (name <$> profile)

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    ((res, form), enctype) <- runFormPost $ profileForm $ Just (Profile $ userName u)
    case res of
         FormSuccess p -> do
            runDB $ update uid [UserName =. (name p)]
            redirect RedirectTemporary ProfileR
         _ -> return ()
    defaultLayout $ do
       setTitle "Profil"
       $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR
