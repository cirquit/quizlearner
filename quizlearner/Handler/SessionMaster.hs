module Handler.SessionMaster where

import Import

-- | Allows abortion of exam creation
getSessionMasterR :: Handler Html
getSessionMasterR = deleteSession "examAttributes" >> redirect QuizcreatorR