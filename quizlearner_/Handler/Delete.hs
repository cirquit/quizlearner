module Handler.Delete where

import Widgets (titleWidget, iconWidget, leftWidget, postWidget)
import Assets (checkTextField)
import Import

checkTitle :: Text ->  Html -> MForm Handler (FormResult Text, Widget)
checkTitle title token = do
  (textResult, textView) <- mreq (checkTextField title) "" Nothing
  let widget = [whamlet|
      #{token}
              <span class=simpleWhite> _{MsgConfirmDelExam}
              <br>
              <span class=simpleWhite> _{MsgConfirmDelExam2}
              <br>
              <span style="margin:10px; color:black;"> ^{fvInput textView}
          <input type=submit value=_{MsgDelExam}>
               |]
  return (textResult, widget)

getDeleteR :: ExamId -> Handler Html
getDeleteR examId = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    exam <-  runDB $ get404 examId
    (widget, enctype) <- generateFormPost $ checkTitle $ examTitle exam
    let middleWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "delete")

postDeleteR :: ExamId -> Handler Html
postDeleteR examId = do
    exam <-  runDB $ get404 examId
    ((res, widget), enctype) <- runFormPost $ checkTitle $ examTitle exam
    case res of
        FormSuccess _ -> do runDB $ delete examId
                            let middleWidget = [whamlet|
                                <span class=simpleWhite> _{MsgSuccessDelete $ examTitle exam}
                                <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                                               |]
                            entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                            defaultLayout $ do $(widgetFile "delete")
        _             -> do let middleWidget = [whamlet|
                                <form method=post enctype=#{enctype}>
                                    ^{widget}
                                <span class=sadred> _{MsgTitleMisMatch}
                                               |]
                            entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                            defaultLayout $ do $(widgetFile "delete")

