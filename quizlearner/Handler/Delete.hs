module Handler.Delete where

import Assets (checkTextField)
import Import
import Widgets (titleWidget, iconWidget, leftWidget, postWidget,
                autoFocusById)


-- | Checks if the input matches the target exam title
checkTitle :: Text ->  Html -> MForm Handler (FormResult Text, Widget)
checkTitle title token = do
    (textResult, textView) <- mreq (checkTextField title MsgTitleMisMatch) "" Nothing
    let widget = [whamlet|
        #{token}
                <span class=simpleWhite> _{MsgConfirmDelExam}
                <br>
                <span class=simpleWhite> _{MsgConfirmDelExam2}
                <br>
                <span style="margin:10px; color:black;"> ^{fvInput textView}
            <input type=submit value=_{MsgDelExam}>
                 |] >> autoFocusById (fvId textView)
    return (textResult, widget)

-- | Checks for login
--   Logged in -> Asks for exam title
--   otherwise -> Prompts to login
getDeleteR :: ExamId -> Handler Html
getDeleteR examId = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    case memail of
       (Just _) -> do
               (entityExamList, exam) <- runDB $ do
                   entityExamList     <- selectList [] [Asc ExamTitle]
                   exam               <- get404 examId
                   return (entityExamList, exam)
               (widget, enctype) <- generateFormPost $ checkTitle $ examTitle exam
               let middleWidget = postWidget enctype widget
               defaultLayout $(widgetFile "delete")
       (_)      -> redirect AccManagerR

-- | Deletes exam if input matches the title
postDeleteR :: ExamId -> Handler Html
postDeleteR examId = do
    exam <- runDB $ get404 examId
    ((res, widget), enctype) <- runFormPost $ checkTitle $ examTitle exam
    case res of
        (FormSuccess _) -> do
                let middleWidget = [whamlet|
                        <span class=simpleWhite> _{MsgSuccessDelete $ examTitle exam}
                        <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                        <meta http-equiv="refresh" content="2;URL='http://localhost:3000/'"/>
                                       |]
                entityExamList <- runDB $ do
                    delete examId
                    entityExamList <- selectList [] [Asc ExamTitle]
                    return entityExamList
                defaultLayout $(widgetFile "delete")
        (_)             -> do
                let middleWidget = postWidget enctype widget >> [whamlet| <span class=sadred> _{MsgTitleMisMatch}|]
                entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                defaultLayout $(widgetFile "delete")

