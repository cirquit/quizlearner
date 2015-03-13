module Handler.Delete where

import Widgets (titleWidget, iconWidget, leftWidget, postWidget)
import Assets (checkTextField)
import Import

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
               |] >> [whamlet|
                      <script> document.getElementById('#{fvId textView}').focus(); |]
  return (textResult, widget)

getDeleteR :: ExamId -> Handler Html
getDeleteR examId = do
    (entityExamList, exam) <- runDB $ do
        entityExamList <- selectList [] [Asc ExamTitle]
        exam <- get404 examId
        return (entityExamList, exam)
    (widget, enctype) <- generateFormPost $ checkTitle $ examTitle exam
    let middleWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "delete")

postDeleteR :: ExamId -> Handler Html
postDeleteR examId = do
    exam <-  runDB $ get404 examId
    ((res, widget), enctype) <- runFormPost $ checkTitle $ examTitle exam
    memail <- lookupSession "_ID"
    case (res, memail) of
        (FormSuccess _, Just _) -> do let middleWidget = [whamlet|
                                          <span class=simpleWhite> _{MsgSuccessDelete $ examTitle exam}
                                          <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                                                         |]
                                      entityExamList <- runDB $ do
                                          delete examId
                                          entityExamList <- selectList [] [Asc ExamTitle]
                                          return entityExamList
                                      defaultLayout $ do $(widgetFile "delete")
        (_, _)             -> do let middleWidget = [whamlet|
                                     <form method=post enctype=#{enctype}>
                                         ^{widget}
                                     <span class=sadred> _{MsgTitleMisMatch}
                                                    |]
                                 entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                                 defaultLayout $ do $(widgetFile "delete")

