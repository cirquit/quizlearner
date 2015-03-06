module Handler.Quizcreator where

import Import
import Assets (unsignedDoubleField, unsignedIntField,  maybeInt,
               maybeDouble, encodeExamAttributes)
import Widgets (titleWidget, iconWidget, leftWidget, postWidget,
                errorWidget, spacingScript, showExamWidget)
import Data.Text (splitOn)
import Data.List.Split (chunksOf)
import Data.List (cycle)

data ExamAttributes = ExamAttributes {
                    title    :: Text
                  , maxScore :: Int
                  , maxTime  :: Int
                  , passPercentage :: Double
                  , questCount :: Int
                }

getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    mayAttributes  <- lookupSession "examAttributes"
    let generatePost = case mayAttributes of
                           (Just text) -> generateFormPost $ examForm $ toExamAttributes text
                           _           -> generateFormPost $ examAttributesForm
    (widget, enctype) <- generatePost
    let middleWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "quizcreator")

postQuizcreatorR :: Handler Html
postQuizcreatorR = do
    mayAttributes  <- lookupSession "examAttributes"
    case mayAttributes of
       (Just text) -> do ((res, _), _) <- runFormPost $ examForm $ toExamAttributes text
                         case res of
                             (FormSuccess exam) -> do
                                 deleteSession "examAttributes"
                                 _ <- runDB $ insert exam
                                 redirect QuizcreatorR
                             _ -> do
                                  entityExamList <- runDB $ selectList [] [Asc ExamTitle]
                                  let middleWidget = errorWidget "Exam parsing"
                                  defaultLayout $ do $(widgetFile "quizcreator")
       _           -> do ((res, _), _) <- runFormPost examAttributesForm
                         case res of
                             (FormSuccess (ExamAttributes a b c d e)) -> do
                                 let message = encodeExamAttributes a b c d e
                                 setSession "examAttributes" message
                                 redirect QuizcreatorR
                             _ -> do
                                 deleteSession "examAttributes"
                                 redirect QuizcreatorR


examForm :: ExamAttributes -> Html -> MForm Handler ((FormResult Exam), Widget)
examForm (ExamAttributes title maxScore maxTime passPercentage questCount) token = do
  let nTimes :: Int -> [Int]
      nTimes n = [1..n]
  qTextFields <- forM (nTimes questCount) (\_ -> mreq textField "" Nothing)
  aTextFields <- forM (nTimes (4 * questCount)) (\_ -> mreq textField "" Nothing)
  aBoolFields <- forM (nTimes (4 * questCount)) (\_ -> mreq boolField "" (Just False))
  let (qTextResults, qTextViews) = unzip qTextFields
      (aTextResults, aTextViews) = unzip aTextFields
      (aBoolResults, aBoolViews) = unzip aBoolFields
      answerResList =  zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
      questions     = FormSuccess $ zipWith (\(FormSuccess q) as -> Question q as) qTextResults (chunksOf 4 answerResList)
      exam          = Exam title maxScore maxTime passPercentage <$> questions
      answerViews   = zipWith3 (\x y z -> zip3 x y z) (chunksOf 4 aTextViews) (chunksOf 4 aBoolViews) (cycle [(nTimes 4)])
      questionViews = zip3 qTextViews answerViews ([1..]::[Int])
      widget        = [whamlet|
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
                              <td>
                  <tr>
                  <td style="text-align:right;"><input type=submit value="Submit question!">
                      |]
  return (exam, widget)

examAttributesForm :: Html -> MForm Handler ((FormResult ExamAttributes), Widget)
examAttributesForm token = do
    (eTitleResult, eTitleView) <- mreq textField "" Nothing
    (eScoreResult, eScoreView) <- mreq unsignedIntField "" (Just 100)
    (eTimeResult, eTimeView)   <- mreq unsignedIntField "" (Just 60)
    (ePassResult, ePassView)   <- mreq unsignedDoubleField "" (Just 50.0)
    (eCountResult, eCountView) <- mreq unsignedIntField "" (Just 5)
    let examAttributes = ExamAttributes <$> eTitleResult <*> eScoreResult <*> eTimeResult <*> ePassResult <*> eCountResult
        widget = [whamlet|
        #{token}
            <table class=questCreator>
                <tr>
                    <td class=smallWhite> Exam Title: 
                    <td style="color:black"> ^{fvInput eTitleView} 
                <tr>
                    <td class=smallWhite> Maximum Score: 
                    <td style="color:black"> ^{fvInput eScoreView}
                    <td class=smallWhite style="text-align:left;"> Points
                <tr>
                    <td class=smallWhite> Maximum Time:
                    <td style="color:black"> ^{fvInput eTimeView}
                    <td class=smallWhite style="text-align:left;"> Minutes
                <tr>
                    <td class=smallWhite> Passing Percentage:
                    <td span style="color:black"> ^{fvInput ePassView}
                    <td class=smallWhite style="text-align:left;"> %
                <tr>
                    <td class=smallWhite> Number of Questions:
                    <td span style="color:black"> ^{fvInput eCountView}
                <tr>
                    <td>
                    <td style="text-align:right;"><input type=submit value="Start exam creation">
                 |]
    return (examAttributes, widget)


toExamAttributes :: Text -> ExamAttributes
toExamAttributes ((splitOn "($)") -> [a, b, c, d, e])     =  let (Just maxScore)       = maybeInt $ unpack b
                                                                 (Just maxTime)        = maybeInt $ unpack c
                                                                 (Just passPercentage) = maybeDouble $ unpack d
                                                                 (Just questCount)     = maybeInt $ unpack e in
                                                             ExamAttributes a maxScore maxTime passPercentage questCount
toExamAttributes _                                         = ExamAttributes (pack "Error in reading exam cookie") 0 0 0.0 0


