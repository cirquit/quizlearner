module Handler.Upload where

import Assets (getAllExams)
import Data.Conduit.Binary
import Data.ByteString.Lazy.Internal as LB
import qualified Data.Text.Lazy.Encoding as LT
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget, postWidget)
import XMLParsing

-- | Upload form
getUploadR :: Handler Html
getUploadR = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    (widget, enctype) <- generateFormPost fileMForm
    let middleWidget = postWidget enctype widget
    defaultLayout $(widgetFile "upload")

-- | Extracts bytestring from submitted file and parses it into an exam
--   Shows error message if parsing fails
postUploadR :: Handler Html
postUploadR = do
    memail <- lookupSession "_ID"
    ((result, widget), enctype) <- runFormPost fileMForm
    case result of
        (FormSuccess fileInfo)   -> do
                bytestring <- runResourceT $ fileSource fileInfo $$ sinkLbs
                maybeExam  <- liftIO $ tryXMLEvaluation bytestring
                case maybeExam of
                    Just newExam -> do
                            (publicExams, privateExams) <- runDB $ do
                                _ <- insert $ newExam memail
                                (publicExams, privateExams) <- getAllExams memail
                                return (publicExams, privateExams)
                            let middleWidget = [whamlet| <span class=simpleWhite>_{MsgFileRec $ fileName fileInfo}|]
                                                >> postWidget enctype widget
                            defaultLayout $(widgetFile "upload")
                    Nothing      -> do
                            (publicExams, privateExams) <- runDB $ getAllExams memail
                            let middleWidget = [whamlet|<div style="margin: 20px;">
                                                         <span class=simpleWhite> _{MsgErrInXML_P1}
                                                             <a href=@{ExampleXMLR} style="font-weight:bold; color:#FFA500;"> _{MsgErrInXML_P2}
                                               |] >> postWidget enctype widget
                            defaultLayout $(widgetFile "upload")
        (_)                     -> do
                (publicExams, privateExams) <- runDB $ getAllExams memail
                let middleWidget = [whamlet|<span class=smallWhite> _{MsgChooseXML}|]
                                   >> postWidget enctype widget
                defaultLayout $(widgetFile "upload")

-- | Basic file input form
fileMForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
fileMForm token = do
    (fileResult, fileView) <- mreq fileField "File" Nothing
    let widget = [whamlet|
        #{token}
            <div style="margin: 20px">
                   <div style="text-align:center;"> <a href=@{ExampleXMLR} class=exampleXML> _{MsgExampleXML} </a>
                   <span class=smallWhite> ^{fvInput fileView}
                   <input type=submit value=_{MsgUpload}>
                 |]
    return (fileResult, widget)

-- | Parses bytestring into exam
tryXMLEvaluation :: LB.ByteString -> IO (Maybe (Maybe Text -> Exam))
tryXMLEvaluation bytestring = do
    let eitherText = LT.decodeUtf8' bytestring
    case eitherText of
         (Right (checkDtd -> Right txt)) -> parseXml $ unpack txt
         _                               -> return Nothing
