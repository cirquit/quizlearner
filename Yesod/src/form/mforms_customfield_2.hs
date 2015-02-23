{-# LANGUAGE GADTs, ViewPatterns, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Main where

import Yesod
import Data.Text as T hiding (zip)
import Control.Applicative
import Control.Monad (forM)
import Yesod.Form
import Data.String


main :: IO ()
main = warp 3000 CarApp


data CarApp = CarApp

instance Yesod CarApp

instance RenderMessage CarApp FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "CarApp" [parseRoutes|
/          HomeR GET
|]


passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password>
            <div>Confirm:
            <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }

getHomeR :: Handler Html
getHomeR = do
    ((res, widget), enctype) <- runFormGet $ renderDivs
        $ areq passwordConfirmField "Password" Nothing
    defaultLayout
        [whamlet|
            <p>Result: #{show res}
            <form enctype=#{enctype}>
                ^{widget}
                <input type=submit value="Change password">
        |]