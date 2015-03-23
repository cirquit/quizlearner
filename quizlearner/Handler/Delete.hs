module Handler.Delete where

import Assets (getAllExams, checkTitleField)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget,
                postWidget, privateExamWidget)

-- | Checks for login
--   Logged in -> Asks for exam title
--   otherwise -> Prompts to login
getDeleteR :: ExamId -> Handler Html
getDeleteR examId = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    case memail of
       (Just _) -> do
               (publicExams, privateExams, exam) <- runDB $ do
                   (publicExams, privateExams)   <- getAllExams memail
                   exam                          <- get404 examId
                   return (publicExams, privateExams, exam)
               (widget, enctype) <- generateFormPost $ checkTitleField $ examTitle exam
               let middleWidget = [whamlet|
                              <span class=simpleWhite> _{MsgConfirmDelExam}
                              <br>
                              <span class=simpleWhite> _{MsgConfirmDelExam2}
                              <br>|] >> postWidget enctype widget
               defaultLayout $(widgetFile "delete")
       (_)      -> redirect AccManagerR

-- | Deletes exam if input matches the title
postDeleteR :: ExamId -> Handler Html
postDeleteR examId = do
    exam <- runDB $ get404 examId
    ((res, widget), enctype) <- runFormPost $ checkTitleField $ examTitle exam
    memail <- lookupSession "_ID"
    case res of
        (FormSuccess _) -> do
                let middleWidget = [whamlet|
                        <span class=simpleWhite> _{MsgSuccessDelete $ examTitle exam}
                        <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                        <meta http-equiv="refresh" content="2;URL='http://localhost:3000/'"/>
                                       |]
                (publicExams, privateExams) <- runDB $ delete examId >> getAllExams memail
                defaultLayout $(widgetFile "delete")
        (_)             -> do
                let middleWidget = [whamlet|
                              <span class=simpleWhite> _{MsgConfirmDelExam}
                              <br>
                              <span class=simpleWhite> _{MsgConfirmDelExam2}
                              <br>|] >> postWidget enctype widget
                                     >> [whamlet| <span class=sadred> _{MsgTitleMisMatch}|]
                (publicExams, privateExams) <- runDB $ getAllExams memail
                defaultLayout $(widgetFile "delete")

