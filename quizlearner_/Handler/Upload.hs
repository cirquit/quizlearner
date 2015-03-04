module Handler.Upload where

import Assets (titleWidget, iconWidget, leftWidget)
import Import
import XMLParsing
import Data.Conduit

form :: Html -> MForm Handler (FormResult FileInfo, Widget)
form = renderDivs $ fileAFormReq "File"


getUploadR :: Handler Html
getUploadR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    ((_, widget), enctype) <- runFormPost form
    let formWidget = [whamlet|
                         <form method=post enctype=#{enctype}>
                             ^{widget}
                             <p>
                             <input type=submit>
                     |]
    defaultLayout $ do $(widgetFile "upload")

-- f :: MonadResource m => Source m ByteString -> ByteString
-- f (Data.Conduit.ConduitM _ ) = error "..."

postUploadR :: Handler Html
postUploadR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    let formWidget = [whamlet|
                         $maybe file <- msubmission
                             <p>File received: #{show 5}
                         <form method=post enctype=#{enctype}>
                             ^{widget}
                             <p>
                             <input type=submit>
                     |]
    defaultLayout $ do $(widgetFile "upload")