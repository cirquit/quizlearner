module Handler.Upload where

import Widgets (titleWidget, iconWidget, leftWidget, postWidget)
import Import
import Data.Conduit.Binary
import Data.ByteString.Lazy.Internal as LB
import qualified Data.Text.Lazy.Encoding as LT
import XMLParsing

getUploadR :: Handler Html
getUploadR = do
    entityExamList    <- runDB $ selectList [] [Asc ExamTitle]
    (widget, enctype) <- generateFormPost fileMForm
    let formWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    ((result, widget), enctype) <- runFormPost fileMForm
    case result of
        FormSuccess fileInfo -> do bytestring <- runResourceT $ fileSource fileInfo $$ sinkLbs
                                   case tryXMLEvaluation bytestring of
                                       Just newExam -> do
                                               _     <- runDB $ insert newExam
                                               entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                                               let formWidget = [whamlet| <span class=simpleWhite>_{MsgFileRec $ fileName fileInfo}|]
                                                                 >> postWidget enctype widget
                                               defaultLayout $ do $(widgetFile "upload")
                                       Nothing      -> do
                                               entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                                               let formWidget = [whamlet|<span class=smallWhite> _{MsgErrInXML_P1}
                                                                             <a href=@{ExampleXMLR} style="font-weight:bold;"> _{MsgErrInXML_P2}
                                                                |] >> postWidget enctype widget
                                               defaultLayout $ do $(widgetFile "upload")
        _                       -> do
                                      entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                                      let formWidget = [whamlet|<span class=smallWhite> _{MsgChooseXML}|]
                                                       >> postWidget enctype widget
                                      defaultLayout $ do $(widgetFile "upload")

fileMForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
fileMForm token = do
    (fileResult, fileView) <- mreq fileField "File" Nothing
    let widget = [whamlet|
        #{token}
            <div style="margin: 20px">
                   <span class=simpleWhite> ^{fvInput fileView}
                   <input type=submit value=_{MsgUpload}>
                 |]
    return (fileResult, widget)

tryXMLEvaluation :: LB.ByteString -> Maybe Exam
tryXMLEvaluation bytestring = do
    let eitherText = LT.decodeUtf8' bytestring
    case eitherText of
         (Right text) -> makeExam $ encodeUtf8 text
         (Left  _)    -> Nothing