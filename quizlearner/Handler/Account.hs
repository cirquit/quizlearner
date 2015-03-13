module Handler.Account where

import Import
import Widgets
import Assets
import Web.Authenticate.BrowserId


getAccountR :: Text -> Handler Html
getAccountR assertion = do
    manager <- newManager
    entityExamList <- runDB (selectList [] [Asc ExamTitle])
    memail <- checkAssertion "localhost:3000" assertion manager
    case memail of
        (Just email) -> do setSession "_ID" email
                           redirectUltDest HomeR
        _            -> do deleteSession "_ID"
                           redirectUltDest HomeR

postAccountR :: Text -> Handler Html
postAccountR _ = do
    deleteSession "_ID"
    redirectUltDest HomeR

    --let middleWidget =  [whamlet|
    --    <span class=simpleWhite>You tried to log in, let's see if it worked.
    --        $maybe email <- memail
    --            <br> Yes it did! You are: #{email}
    --        $nothing
    --            <br> Nope, sorry
    --                    |]
    --defaultLayout $ do
    --    $(widgetFile "account")