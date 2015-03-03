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
                                                   <p class=simpleWhite> #{content}
                                                      $forall (Answer content isCorrect) <- list
                                                          <p class=simpleWhite> #{content} is #{show isCorrect}
                                                  |]
         _                                     -> [whamlet| <span class=simpleWhite> Something went wrong...|]
    defaultLayout $ do $(widgetFile "quizcreator")

questionForm :: Html -> MForm Handler ((FormResult Question), Widget)
questionForm token = do
  let fourTimes = [1..4] :: [Int]
  (qNameResult, qNameView) <- mreq textField "" Nothing
  aTextFields <- forM fourTimes (\_ -> mreq textField "" Nothing)
  aBoolFields <- forM fourTimes (\_ -> mreq boolField "" (Just False))
  let (aTextResults, aTextViews) = unzip aTextFields
  let (aBoolResults, aBoolViews) = unzip aBoolFields
  let answerList = FormSuccess $ zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
  let answerViews = zip3 aTextViews aBoolViews fourTimes
  let quest = Question <$> qNameResult <*> answerList
  let widget = [whamlet|
                  #{token}
                          <ul class=questCreator>
                              <li class=simpleWhite>Questiontext <span style="color:black"> ^{fvInput qNameView} </span>
                              $forall (tview, bview, c) <- answerViews
                                  <li class=simpleWhite>Answer Nr.#{show c} <span style="color:black"> ^{fvInput tview} </span>
                                                        Is it correct?      <span> ^{fvInput bview} </span>
                        <input type=submit value="Submit question!">
               |]
  return (quest, widget)
