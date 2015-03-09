module Handler.Lang where

import Import

getLangR :: Handler Html
getLangR = redirectUltDest HomeR

postLangR :: Handler Html
postLangR = do
    let validLangs = ["en", "de", "ru"]
    lang <- runInputPost $ ireq textField "lang"
    case lang `elem` validLangs of
        True -> setLanguage lang >> redirectUltDest HomeR
        _    -> redirectUltDest HomeR

