module Handler.Upload where

import Widgets (titleWidget, iconWidget, leftWidget, postWidget)
import Import
import Data.Conduit.Binary
--import qualified Data.ByteString.Lazy as LB
--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Encoding as LT
--
-- import Data.Maybe
-- import qualified Data.ByteString.Lazy as LB
-- import Data.Default
-- import Data.Text (Text)
-- import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Exception hiding (Handler(), try)
--import Text.Blaze
import XMLParsing
--import Data.Conduit

form :: Html -> MForm Handler (FormResult FileInfo, Widget)
form = renderDivs $ fileAFormReq "File"


getUploadR :: Handler Html
getUploadR = do
    entityExamList         <- runDB $ selectList [] [Asc ExamTitle]
    ((_, widget), enctype) <- runFormPost form
    let formWidget = [whamlet|
        <div style="margin: 20px">
           <form method=post enctype=#{enctype}>
               <span class=simpleWhite> ^{widget}
               <input type=submit value="Upload">
                     |]
    defaultLayout $ do $(widgetFile "upload")

postUploadR :: Handler Html
postUploadR = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    ((result, widget), enctype) <- runFormPost form
    case result of
        FormSuccess res -> do
            bytes <- runResourceT $ fileSource res $$ sinkLbs
            text  <- liftIO $ preview bytes
            let subm = makeExam $ encodeUtf8 text
            case subm of 
                Just newExam -> do 
                    _     <- runDB $ insert newExam
                    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                    let formWidget = [whamlet|
                        <div style="margin: 20px">
                                 <span class=simpleWhite> File received: #{fileName res}
                             <form method=post enctype=#{enctype}>
                                 <span class=simpleWhite> ^{widget}
                                 <input type=submit value="Upload">
                                     |]
                    defaultLayout $ do $(widgetFile "upload")
                Nothing      -> do 
                        let formWidget = [whamlet|<span class=smallWhite> Hi|]
                        defaultLayout $ do $(widgetFile "upload")
    
       
        _               -> do 
                        let formWidget = [whamlet|<span class=smallWhite> Please choose an XML file to upload.
                                         |] >> postWidget enctype widget
                        defaultLayout $ do $(widgetFile "upload")


-- preview :: Data.ByteString.Lazy.Internal.ByteString -> IO LT.Text
preview bytes = do
    eText        <- try . evaluate $ LT.decodeUtf8 bytes :: IO (Either SomeException LT.Text)
    return $ case eText of
      Left _     -> LT.pack "This is not happening"
      Right text -> text
