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


checkBoxWidget :: T.Text -> T.Text -> Widget
checkBoxWidget check name = if (T.unpack check) == "True" then toWidget [hamlet| <input type=checkbox name=#{T.unpack name} checked>    |]
                                                          else toWidget [hamlet| <input type=checkbox name=#{T.unpack name} unchecked>  |]

-- Display the set name form
getSetNameR :: Handler Html
getSetNameR = do
    mname <- lookupSession "name"
    mcheck1 <- lookupSession "check1"
    mcheck2 <- lookupSession "check2"
    mcheck3 <- lookupSession "check3"
    mcheck4 <- lookupSession "check4"
    case (mname, mcheck1, mcheck2, mcheck3, mcheck4) of
        (Just name, Just check1, Just check2, Just check3, Just check4) -> defaultLayout [whamlet| <form method=post>
                                                My name is #
                                                <input type=text name=name value=#{fromJust mname}>
                                                 . #
                                                 ^{checkBoxWidget check1 (T.pack "check1")}
                                                 ^{checkBoxWidget check2 (T.pack "check2")}
                                                 ^{checkBoxWidget check3 (T.pack "check3")}
                                                 ^{checkBoxWidget check4 (T.pack "check4")}
                                                <input type=submit value="Set name n' check">
                                    |]
        (_,_,_,_,_)   -> defaultLayout [whamlet| <form method=post>
                                                    My name is #
                                                    <input type=text name=name>
                                                    . #
                                                     ^{checkBoxWidget (T.pack "False") (T.pack "check1")}
                                                     ^{checkBoxWidget (T.pack "False") (T.pack "check2")}
                                                     ^{checkBoxWidget (T.pack "Flase") (T.pack "check3")}
                                                     ^{checkBoxWidget (T.pack "False") (T.pack "check4")}
                                                    <input type=submit value="Set name n' check">
                                    |]

-- Retreive the submitted name from the user
postSetNameR :: Handler ()
postSetNameR = do
    -- Get the submitted name and set it in the session
    name <- runInputPost $ ireq textField "name"
    check1 <- runInputPost $ ireq checkBoxField "check1"
    check2 <- runInputPost $ ireq checkBoxField "check2"
    check3 <- runInputPost $ ireq checkBoxField "check3"
    check4 <- runInputPost $ ireq checkBoxField "check4"
    setSession "name" name
    setSession "check1" $ T.pack $ show check1
    setSession "check2" $ T.pack $ show check2
    setSession "check3" $ T.pack $ show check3
    setSession "check4" $ T.pack $ show check4

    -- After we get a name, redirect to the ultimate destination.
    -- If no destination is set, default to the homepage
    redirectUltDest HomeR

getSayHelloR :: Handler Html
getSayHelloR = do
    -- Lookup the name value set in the session
    mname <- lookupSession "name"
    mcheck1 <- lookupSession "check1"
    mcheck2 <- lookupSession "check2"
    mcheck3 <- lookupSession "check3"
    mcheck4 <- lookupSession "check4"
    case (mname,mcheck1,mcheck2,mcheck3,mcheck4) of

         (Just name, Just check1, Just check2, Just check3, Just check4)-> defaultLayout [whamlet|<p>Welcome #{name}, the box Nr1. was set on #{show check1} <br>
                                                                                                                      the box Nr2. was set on #{show check2} <br>
                                                                                                                      the box Nr3. was set on #{show check3} <br>
                                                                                                                      the box Nr4. was set on #{show check4} <br>
                                                                                        |]
         (_,_,_,_,_) -> do
            -- No name in the session, set the current page as
            -- the ultimate destination and red
    -- After we get a name, redirect to thirect to the
            -- SetName page
            setUltDestCurrent
            setMessage "Please tell me your name"
            redirect SetNameR

main :: IO ()
main = warp 3000 App