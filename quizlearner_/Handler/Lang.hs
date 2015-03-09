module Handler.Lang where

import Import

postLangR :: Handler Html
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirectUltDest HomeR
