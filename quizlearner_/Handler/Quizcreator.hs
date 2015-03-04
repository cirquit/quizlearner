module Handler.Quizcreator where

import Import
import Assets (titleWidget, iconWidget, leftWidget, unsignedDoubleField,
               unsignedIntField,  maybeInt, maybeDouble, encodeExamAttributes,
               createExam, postWidget, errorWidget, spacingScript)
import Data.Text (splitOn)
import Data.List.Split (chunksOf)
import Data.List (cycle)

data TempExam = TempExam {
                    title    :: Text
                  , maxScore :: Int
                  , maxTime  :: Int
                  , passPercentage :: Double
                  , questCount :: Int
                }

getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    mayAttributes  <- lookupSession "examAttributes"
    let generatePost = case mayAttributes of
                           (Just text) -> generateFormPost $ examForm $ toTempExam text
                           _           -> generateFormPost $ examAttributesForm
    (widget, enctype) <- generatePost
    let middleWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "quizcreator")

postQuizcreatorR :: Handler Html
postQuizcreatorR = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    mayAttributes  <- lookupSession "examAttributes"
    case mayAttributes of
       (Just text) -> do ((res, _), _) <- runFormPost $ examForm $ toTempExam text
                         case res of
                             (FormSuccess exam) -> do
                                 let middleWidget =  createExam exam
                                 deleteSession "examAttributes"
                                 defaultLayout $ do $(widgetFile "quizcreator")
                             _ -> do
                                 let middleWidget = errorWidget $ pack "Exam parsing"
                                 defaultLayout $ do $(widgetFile "quizcreator")
       _           -> do ((res, _), _) <- runFormPost examAttributesForm
                         case res of
                             (FormSuccess (TempExam a b c d e)) -> do
                                 let message = encodeExamAttributes a b c d e
                                 setSession "examAttributes" message
                                 redirect QuizcreatorR
                             _ -> do
                                 deleteSession "examAttributes"
                                 redirect QuizcreatorR


examForm :: TempExam -> Html -> MForm Handler ((FormResult Exam), Widget)
examForm (TempExam title maxScore maxTime passPercentage questCount) token = do
  let fourTimes = [1..4] :: [Int]
  let qTimes = [1..questCount] :: [Int]
  let nTimes = [1..4*questCount] :: [Int]
  qTextFields <- forM qTimes (\_ -> mreq textField "" Nothing)
  aTextFields <- forM nTimes (\_ -> mreq textField "" Nothing)
  aBoolFields <- forM nTimes (\_ -> mreq boolField "" (Just False))
  let (qTextResults, qTextViews) = unzip qTextFields
  let (aTextResults, aTextViews) = unzip aTextFields
  let (aBoolResults, aBoolViews) = unzip aBoolFields
  let answerResList =  zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
  let questions = FormSuccess $ zipWith (\(FormSuccess q) as -> Question q as) qTextResults (chunksOf 4 answerResList)
  let exam = Exam title maxScore maxTime passPercentage <$> questions
  let answerViews = zipWith3 (\x y z -> zip3 x y z) (chunksOf 4 aTextViews) (chunksOf 4 aBoolViews) (cycle [fourTimes])-- [[(t1,b1,1),(t2,b2,2),(t3,b3,3), (t4,b4,4)],...]
  let questionViews = zip3 qTextViews answerViews ([1..]::[Int]) -- [(q1, [(t1,b2,1),(t2,b2,2),(t3,b3,3),(t4,b4,4)]),...]
  let widget = [whamlet|
      #{token}
              <table class=questCreator>
                  $forall (qView, aList,n) <- questionViews
                          <tr>
                              <td class=questionTD>Question Nr. #{show n}: 
                              <td style="color:black"> ^{fvInput qView}
                      $forall (tview, bview, c) <- aList
                          <tr>
                              <td class=smallWhite>Answer Nr.#{show c}:
                              <td style="color:black;"> ^{fvInput tview}
                              <td class=smallWhite>Is it correct?
                              <td class=smallWhite>^{fvInput bview}
            <input type=submit value="Submit question!">
               |]
  return (exam, widget)

examAttributesForm :: Html -> MForm Handler ((FormResult TempExam), Widget)
examAttributesForm token = do
  (eTitleResult, eTitleView) <- mreq textField "" Nothing
  (eScoreResult, eScoreView) <- mreq unsignedIntField "" (Just 100)
  (eTimeResult, eTimeView)   <- mreq unsignedIntField "" (Just 60)
  (ePassResult, ePassView)   <- mreq unsignedDoubleField "" (Just 50.0)
  (eCountResult, eCountView) <- mreq unsignedIntField "" (Just 5)
  let tempExam = TempExam <$> eTitleResult <*> eScoreResult <*> eTimeResult <*> ePassResult <*> eCountResult
  let widget = [whamlet|
      #{token}
          <ul class=questCreator>
              <li class=simpleWhite> Examtitle <span style="color:black"> ^{fvInput eTitleView} </span>
              <li class=simpleWhite> Max. exam score <span style="color:black"> ^{fvInput eScoreView} </span>p
              <li class=simpleWhite> Max. time <span style="color:black"> ^{fvInput eTimeView} </span>min
              <li class=simpleWhite> Passing percentage <span style="color:black"> ^{fvInput ePassView}</span>%
              <li class=simpleWhite> Number of questions <span style="color:black"> ^{fvInput eCountView} </span>
      <input type=submit value="Start exam creaton!">
               |]
  return (tempExam, widget)


toTempExam :: Text -> TempExam
toTempExam (splitOn (pack " ") -> [a, b, c, d, e]) = let (Just maxScore)       = maybeInt $ unpack b
                                                         (Just maxTime)        = maybeInt $ unpack c
                                                         (Just passPercentage) = maybeDouble $ unpack d
                                                         (Just questCount)     = maybeInt $ unpack e in
                                                     TempExam a maxScore maxTime passPercentage questCount
toTempExam _                                       = TempExam (pack "Error in reading exam cookie") 0 0 0.0 0
