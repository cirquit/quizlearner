module Handler.Delete where

import Assets (titleWidget, iconWidget, leftWidget, checkTextField, postWidget)
import Import


checkTitle :: Text ->  Html -> MForm Handler (FormResult Text, Widget)
checkTitle title token = do
  (textResult, textView) <- mreq (checkTextField title) "" Nothing
  let widget = [whamlet|
          #{token}
                 <span class=simpleWhite> Do you really want to delete this exam?
                 <br>
                 <span class=simpleWhite> Please type the examtitle correctly and click delete.
                 <br>
                 <span style="margin:10px; color:black;"> ^{fvInput textView}
              <input type=submit value="Delete exam!">
               |]
  return (textResult, widget)

getDeleteR :: ExamId -> Handler Html
getDeleteR examId = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
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
                               <span class=simpleWhite> You successfully deleted #{examTitle exam}!
                               <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> Get back! </label>
                                               |]
                            entityExamList <- runDB $ selectList [] [Desc ExamTitle]
                            defaultLayout $ do $(widgetFile "delete")
        _             -> do let middleWidget = [whamlet|
                                <form method=post enctype=#{enctype}>
                                    ^{widget}
                                <span class=sadred> Sorry, the input does not match the title
                                               |]
                            entityExamList <- runDB $ selectList [] [Desc ExamTitle]
                            defaultLayout $ do $(widgetFile "delete")

