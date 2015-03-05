module Handler.Upload where

import Widgets (titleWidget, iconWidget, leftWidget)
import Import
--import XMLParsing
--import Data.Conduit

form :: Html -> MForm Handler (FormResult FileInfo, Widget)
form = renderDivs $ fileAFormReq "File"


getUploadR :: Handler Html
getUploadR = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    ((_, widget), enctype) <- runFormPost form
    let formWidget = [whamlet|
        <div style="margin: 20px">
           <form method=post enctype=#{enctype}>
               <span class=simpleWhite> ^{widget}
               <input type=submit value="Upload!">
                     |]
    defaultLayout $ do $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
                          FormSuccess res -> Just res
                          _               -> Nothing
    let formWidget = [whamlet|
        <div style="margin: 20px">
             $maybe file <- msubmission
                 <span class=simpleWhite> File received: #{fileName file}
             <form method=post enctype=#{enctype}>
                 <span class=simpleWhite> ^{widget}
                 <input type=submit value="Upload!">
                     |]
    defaultLayout $ do $(widgetFile "upload")