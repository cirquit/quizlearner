module Handler.Account where

import Import
import Web.Authenticate.BrowserId


getAccountR :: Text -> Handler Html
getAccountR assertion = do
    manager <- newManager
    memail <- checkAssertion "localhost:3000" assertion manager
    case memail of
        (Just email) -> do setSession "_ID" email
                           redirectUltDest HomeR
        _            -> do deleteSession "_ID"
                           redirectUltDest HomeR

postAccountR :: Text -> Handler Html
postAccountR _ = do
    deleteSession "_ID"
    redirect HomeR