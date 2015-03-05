module Handler.Upload where

import Widgets (titleWidget, iconWidget, leftWidget)
import Import
import Data.Conduit.Binary
--import qualified Data.ByteString.Lazy as LB
--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Encoding as LT
--
import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
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
    bytes   <- runResourceT $ fileSource (fromJust msubmission) $$ sinkLbs
    text <- liftIO $ preview bytes
    runDB $ insert $ makeExam $ encodeUtf8 text
    let formWidget = [whamlet|
        <div style="margin: 20px">
             $maybe file <- msubmission
                 <span class=simpleWhite> File received: #{fileName file}
             <form method=post enctype=#{enctype}>
                 <span class=simpleWhite> ^{widget}
                 <input type=submit value="Upload!">
                     |]
    defaultLayout $ do $(widgetFile "upload")



preview bytes = do
    eText <- try . evaluate $ LT.decodeUtf8 bytes :: IO (Either SomeException LT.Text)
    return $ case eText of
      Left _ -> LT.pack "This is not happening"
      Right text -> text
