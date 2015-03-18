module Handler.SetPublic where

import Assets (checkTitleField, getAllExams)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget,
                postWidget, privateExamWidget, errorWidget)

getSetPublicR :: ExamId -> Handler Html
getSetPublicR examId = do
    memail <- lookupSession "_ID"
    (publicExams, privateExams, exam) <- runDB $ do
                   (publicExams, privateExams)   <- getAllExams memail
                   exam                          <- get404 examId
                   return (publicExams, privateExams, exam)
    setUltDestCurrent
    case (memail, examAuthor exam) of
       (Just email, Just author) -> do
               case email == author of
                   True  -> do (widget, enctype) <- generateFormPost $ checkTitleField $ examTitle exam
                               let middleWidget = [whamlet|
                                   <span class=simpleWhite> _{MsgConfirmPublic}
                                   <br>
                                   <span class=simpleWhite> _{MsgConfirmPublic2}
                                   <br>           |] >>postWidget enctype widget
                               defaultLayout $(widgetFile "setpublic")
                   False -> do let middleWidget = errorWidget "You are not authorized to make this exam public"
                               defaultLayout $(widgetFile "setpublic")
       (_, _)      -> redirect AccManagerR

postSetPublicR :: ExamId -> Handler Html
postSetPublicR examId = do
    exam <- runDB $ get404 examId
    ((res, widget), enctype) <- runFormPost $ checkTitleField $ examTitle exam
    memail <- lookupSession "_ID"
    case (res, examAuthor exam) of
        (FormSuccess _, Just _) -> do
                let middleWidget = [whamlet|
                        <span class=simpleWhite> _{MsgSetExamPublic $ examTitle exam}
                        <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                        <meta http-equiv="refresh" content="2;URL='http://localhost:3000/'"/>
                                       |]
                (publicExams, privateExams) <- runDB $ do
                    delete examId
                    _ <- insert $ exam {examAuthor=Nothing}
                    (publicExams, privateExams)   <- getAllExams memail
                    return (publicExams, privateExams)
                defaultLayout $(widgetFile "setpublic")
        (FormSuccess _, Nothing)             -> do
                let middleWidget = [whamlet| <span class=sadred> _{MsgExamAlreadyPublic}|]
                (publicExams, privateExams) <- runDB $ getAllExams memail
                defaultLayout $(widgetFile "setpublic")
        (_, _)             -> do
                let middleWidget = postWidget enctype widget >> [whamlet| <span class=sadred> _{MsgTitleMisMatch}|]
                (publicExams, privateExams) <- runDB $ getAllExams memail
                defaultLayout $(widgetFile "setpublic")
