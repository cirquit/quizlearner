module Handler.Upload where

import Assets (loadDB, titleWidget, iconWidget, leftWidget)
import Import

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

postUploadR :: Handler Html
postUploadR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    let formWidget = [whamlet|
                         $maybe file <- msubmission
                             <p>File received: #{fileName file}
                         <form method=post enctype=#{enctype}>
                             ^{widget}
                             <p>
                             <input type=submit>
                     |]
    defaultLayout $ do $(widgetFile "upload")