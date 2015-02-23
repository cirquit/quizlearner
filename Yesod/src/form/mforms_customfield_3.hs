{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Control.Applicative
import Data.Text (Text)

data MFormExample = MFormExample
mkYesod "MFormExample" [parseRoutes|
/ RootR GET
|]

instance Yesod MFormExample
instance RenderMessage MFormExample FormMessage where
renderMessage _ _ = defaultFormMessage

data Person = Person { personName :: Text, personAge :: Text}
  deriving Show

textBoxField :: Text -> Field Handler Text
textBoxField label = Field
  { fieldParse = \rawVals _ ->
         case rawVals of
        [a] -> return $ Right $ Just a
        [] -> return $ Right Nothing
        , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
                [whamlet|
                <div class="form-group">
                <label for=#{idAttr} class="control-label col-lg-4">#{label}
                <div class="col-lg-8">
                <input id=#{idAttr} name=#{nameAttr} *{otherAttrs}
                type="text" class="form-control">
                |]
                , fieldEnctype = UrlEncoded
                }


cPasswordField :: Text -> Field Handler Text
cPasswordField label = Field
 { fieldParse = \rawVals _ ->
          case rawVals of
                [a] -> return $ Right $ Just a
                [] -> return $ Right Nothing
                , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
                    [whamlet|
                    <div class="form-group">
                    <label for=#{idAttr} class="control-label col-lg-4">#{label}
                    <div class="col-lg-8">
                    <input id=#{idAttr} name=#{nameAttr} *{otherAttrs}
                    type="password" class="form-control"
                    data-original-title="Please use your secure password" data-placement="top">
                    |]
          , fieldEnctype = UrlEncoded
}
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm extra = do
    (nameRes, nameView) <- mreq (textBoxField "Normal Input Field") "" Nothing
    (ageRes, ageView) <- mreq (cPasswordField "Password Field") "" Nothing
    let personRes = Person <$> nameRes <*> ageRes
    let widget = do
                    [whamlet|
                    #{extra}
                    ^{fvInput nameView}
                    ^{fvInput ageView}
                    |]
    return (personRes, widget)

getRootR :: Handler Html
getRootR = do
    ((res, widget), enctype) <- runFormGet personForm
    defaultLayout
              [whamlet|
              <p>Result: #{show res}
              <form enctype=#{enctype} class="form-horizontal">
              ^{widget}
              |]


main :: IO ()
main = warp 3000 MFormExample