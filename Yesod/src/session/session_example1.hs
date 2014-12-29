{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Data.Maybe (fromJust)
import qualified Data.Text as T
data App = App

mkYesod "App" [parseRoutes|
/         HomeR     GET
/setname  SetNameR  GET POST
/sayhello SayHelloR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <p>
            <a href=@{SetNameR}>Set your name
        <p>
            <a href=@{SayHelloR}>Say hello
    |]

inputBox'' :: Widget
inputBox'' = do
                mcheck <- lookupSession "check"
                case mcheck of
                  (Just check) -> if (T.unpack check) == "True" then toWidget [hamlet| <input type=checkbox name=check checked>  |]
                                                                else toWidget [hamlet| <input type=checkbox name=check unchecked>|]
                  Nothing -> toWidget [hamlet| <input type=checkbox name=check unchecked>|]

-- Display the set name form
getSetNameR :: Handler Html
getSetNameR = do
    mname <- lookupSession "name"
    mcheck <- lookupSession "check"
    case (mname, mcheck) of
        (Just name, Just check) -> defaultLayout [whamlet| <form method=post>
                                                My name is #
                                                <input type=text name=name value=#{fromJust mname}>
                                                . #
                                                ^{inputBox''}
                                                <input type=submit value="Set name n' check">
                                    |]
        (_,_)   -> defaultLayout [whamlet| <form method=post>
                                               My name is #
                                               <input type=text name=name>
                                               . #
                                               ^{inputBox''}
                                               <input type=submit value="Set name n' check">
                                    |]

-- Retreive the submitted name from the user
postSetNameR :: Handler ()
postSetNameR = do
    -- Get the submitted name and set it in the session
    name <- runInputPost $ ireq textField "name"
    check<- runInputPost $ ireq checkBoxField "check"
    setSession "name" name
    setSession "check" $ T.pack $ show check

    -- After we get a name, redirect to the ultimate destination.
    -- If no destination is set, default to the homepage
    redirectUltDest HomeR

getSayHelloR :: Handler Html
getSayHelloR = do
    -- Lookup the name value set in the session
    mname <- lookupSession "name"
    mcheck <- lookupSession "check"
    case (mname,mcheck) of
         (Nothing,_) -> do
            -- No name in the session, set the current page as
            -- the ultimate destination and red
    -- After we get a name, redirect to thirect to the
            -- SetName page
            setUltDestCurrent
            setMessage "Please tell me your name"
            redirect SetNameR
         (Just name, Just check)-> defaultLayout [whamlet|<p>Welcome #{name}, the box was set on #{show check}|]

main :: IO ()
main = warp 3000 App