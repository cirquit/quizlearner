module Handler.Quizcreator where

import Import
import Assets (titleWidget, iconWidget, leftWidget, unsignedDoubleField, unsignedIntField)
import Data.Text (splitOn)
import Prelude (reads)

data TempExam = TempExam {
                    title    :: Text
                  , maxScore :: Int
                  , maxTime  :: Int
                  , passPercentage :: Double
                  , questCount :: Int
                }

maybeRead :: Read a => String -> Maybe a
maybeRead (reads -> [(x,"")]) = Just x
maybeRead _ = Nothing

maybeInt :: String -> Maybe Int
maybeInt = maybeRead

maybeDouble :: String -> Maybe Double
maybeDouble = maybeRead


toTempExam :: Text -> TempExam
toTempExam (splitOn (pack " ") -> [a, b, c, d, e]) = TempExam a maxScore maxTime passPercentage questCount
  where (Just maxScore)       = maybeInt $ unpack b
        (Just maxTime)        = maybeInt $ unpack c
        (Just passPercentage) = maybeDouble $ unpack d
        (Just questCount)     = maybeInt $ unpack e

--if isValid text then do (widget, enctype) <- generateFormPost examForm $ toTempExam text
                        --              else do deleteSession "examAttributes"

getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    mayAttributes  <- lookupSession "examAttributes"
    case mayAttributes of
     (Just text) -> do (widget, enctype) <- generateFormPost $ examForm $ toTempExam text
                       let middleWidget = [whamlet|
                              <form method=post enctype=#{enctype}>
                                   ^{widget}
                                             |]
                       defaultLayout $ do $(widgetFile "quizcreator")
     _       ->     do (widget, enctype) <- generateFormPost examAttributesForm
                       let middleWidget = [whamlet|
                                <form method=post enctype=#{enctype}>
                                     ^{widget}
                                               |]
                       defaultLayout $ do $(widgetFile "quizcreator")

--if isValid text then ((res, _), _) <- runFormPost examForm $ toTempExam text
--                                      else deleteSession "examAttributes"

postQuizcreatorR :: Handler Html
postQuizcreatorR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    mayAttributes  <- lookupSession "examAttributes"
    case mayAttributes of
     (Just text) -> do ((res, _), _) <- runFormPost $ examForm $ toTempExam text
                       case res of
                         (FormSuccess (Exam title maxScore maxTime passPercentage qList)) -> do let middleWidget = [whamlet|
                                                                                                 <p class=simpleWhite> #{title}
                                                                                                    $forall (Question content aList) <- qList
                                                                                                        <p class=simpleWhite> #{content}
                                                                                                                   |]
                                                                                                deleteSession "examAttributes"
                                                                                                defaultLayout $ do $(widgetFile "quizcreator")
                         _ -> do let middleWidget = [whamlet|
                                                     <span class=simpleWhite> Something went wrong...in exam parsing
                                                 |]
                                 defaultLayout $ do $(widgetFile "quizcreator")

     _           -> do ((res, _), _) <- runFormPost examAttributesForm
                       case res of
                        (FormSuccess (TempExam a b c d e)) -> do let tExam = a ++ pack (' ':show b) ++ pack (' ':show c) ++ pack (' ':show d) ++ pack (' ':show e)
                                                                 setSession "examAttributes" tExam
                                                                 redirect QuizcreatorR
                        _ -> do let middleWidget = [whamlet|
                                                     <span class=simpleWhite> Something went wrong...
                                                 |]
                                defaultLayout $ do $(widgetFile "quizcreator")

examForm :: TempExam -> Html -> MForm Handler ((FormResult Exam), Widget)
examForm (TempExam title maxScore maxTime passPercentage questCount) token = do
  let fourTimes = [1..4] :: [Int]
  (qNameResult, qNameView) <- mreq textField "" Nothing
  aTextFields <- forM fourTimes (\_ -> mreq textField "" Nothing)
  aBoolFields <- forM fourTimes (\_ -> mreq boolField "" (Just False))
  let (aTextResults, aTextViews) = unzip aTextFields
  let (aBoolResults, aBoolViews) = unzip aBoolFields
  let answerList = FormSuccess $ zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
  let answerViews = zip3 aTextViews aBoolViews fourTimes
  let quest = Question <$> qNameResult <*> answerList
  let questList = FormSuccess $ map (\(FormSuccess x) -> x) [quest, quest]
  let exam = Exam title maxScore maxTime passPercentage <$> questList
  let widget = [whamlet|
                  #{token}
                          <ul class=questCreator>
                              <li class=simpleWhite>Questiontext <span style="color:black"> ^{fvInput qNameView} </span>
                              $forall (tview, bview, c) <- answerViews
                                  <li class=simpleWhite>Answer Nr.#{show c} <span style="color:black"> ^{fvInput tview} </span>
                                                        Is it correct?      <span> ^{fvInput bview} </span>
                        <input type=submit value="Submit question!">
               |]
  return (exam, widget)

examAttributesForm :: Html -> MForm Handler ((FormResult TempExam), Widget)
examAttributesForm token = do
  (eTitleResult, eTitleView) <- mreq textField "" Nothing
  (eScoreResult, eScoreView) <- mreq unsignedIntField "" (Just 100)
  (eTimeResult, eTimeView)   <- mreq unsignedIntField "" (Just 60)
  (ePassResult, ePassView)   <- mreq unsignedDoubleField "" (Just 50.0)
  (eCountResult, eCountView) <- mreq unsignedIntField "" Nothing
  let tempExam = TempExam <$> eTitleResult <*> eScoreResult <*> eTimeResult <*> ePassResult <*> eCountResult
  let widget = [whamlet|
                   #{token}
                       <ul class=questCreator>
                           <li class=simpleWhite> Examtitle <span style="color:black"> ^{fvInput eTitleView} </span>
                           <li class=simpleWhite> Max. exam score <span style="color:black"> ^{fvInput eScoreView} </span>
                           <li class=simpleWhite> Max. time <span style="color:black"> ^{fvInput eTimeView} </span>
                           <li class=simpleWhite> Passing percentage <span style="color:black"> ^{fvInput ePassView} %</span>
                           <li class=simpleWhite> Number of questions <span style="color:black"> ^{fvInput eCountView} </span>
                   <input type=submit value="Submit exam!">
               |]
  return (tempExam, widget)