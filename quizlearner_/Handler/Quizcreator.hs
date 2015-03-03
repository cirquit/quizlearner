module Handler.Quizcreator where

import Import
import Assets (titleWidget, iconWidget, leftWidget)

getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    (widget, enctype) <- generateFormPost questionForm
    let middleWidget = [whamlet|
                           <form method=post enctype=#{enctype}>
                                ^{widget}
                       |]
    defaultLayout $ do $(widgetFile "quizcreator")

postQuizcreatorR :: Handler Html
postQuizcreatorR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    ((res, _), _) <- runFormPost questionForm
    let middleWidget = case res of
         (FormSuccess (Question content list)) -> [whamlet|
                                                   <span class=simpleWhite> #{show content}
                                                      $forall (Answer content isCorrect) <- list
                                                          <span class=simpleWhite> #{show content} + #{show isCorrect}
                                                  |]
         _                  -> [whamlet| <span class=simpleWhite> Something went wrong...|]
    defaultLayout $ do $(widgetFile "quizcreator")

questionForm :: Html -> MForm Handler ((FormResult Question), Widget)
questionForm token = do
  let fourTimes = [1..4] :: [Int]
  (qNameResult, qNameView) <- mreq textField "testing...1" Nothing
  aTextFields <- forM fourTimes (\_ -> mreq textField "testing...2" Nothing)
  aBoolFields <- forM fourTimes (\_ -> mreq boolField "testing...3" (Just False))
  let (aTextResults, aTextViews) = unzip aTextFields
  let (aBoolResults, aBoolViews) = unzip aBoolFields
  let answerList = FormSuccess $ zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
  let answerViews = zip3 aTextViews aBoolViews fourTimes
  let quest = Question <$> qNameResult <*> answerList
  let widget = [whamlet|
                  #{token}
                          <ul>
                            <li> Questiontext
                                ^{fvInput qNameView}
                            $forall (tview, bview, c) <- answerViews
                                <li> Answer Nr.#{show c} ^{fvInput tview}
                                     Is it correct?      ^{fvInput bview}
                        <input type=submit value="Submit question!">
               |]
  return (quest, widget)
