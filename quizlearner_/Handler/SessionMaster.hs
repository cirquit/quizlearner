module Handler.SessionMaster where

import Import

getSessionMasterR :: Handler Html
getSessionMasterR = deleteSession "examAttributes" >> redirectUltDest HomeR