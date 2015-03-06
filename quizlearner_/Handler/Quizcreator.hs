module Handler.Quizcreator where

import Import
import Assets (unsignedDoubleField, unsignedIntField,  maybeInt,
               maybeDouble, encodeExamAttributes)
import Widgets (titleWidget, iconWidget, leftWidget, postWidget,
                errorWidget, spacingScript)
import Data.Text (splitOn)
import Data.List.Split (chunksOf)
import Data.List (cycle)

data ExamAttributes = ExamAttributes {
                    title    :: Text
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
                                 (FormSuccess (ExamAttributes a b c)) -> do
                                     setSession "examAttributes" $ encodeExamAttributes a b c
                                     redirect QuizcreatorR
                                 _ -> do
                                     deleteSession "examAttributes"
                                     redirect QuizcreatorR

examForm :: ExamAttributes -> Html -> MForm Handler ((FormResult Exam), Widget)
examForm (ExamAttributes title passPercentage questCount) token = do
  qTextFields <- replicateM questCount  (mreq textField "" Nothing)
  aTextFields <- replicateM (4 * questCount) (mreq textField "" Nothing)
  aBoolFields <- replicateM (4 * questCount) (mreq boolField "" (Just False))
  let (qTextResults, qTextViews) = unzip qTextFields
      (aTextResults, aTextViews) = unzip aTextFields
      (aBoolResults, aBoolViews) = unzip aBoolFields
      answerResList =  zipWith (\(FormSuccess x) (FormSuccess y) -> Answer x y) aTextResults aBoolResults
      questions     = FormSuccess $ zipWith (\(FormSuccess q) as -> Question q as) qTextResults (chunksOf 4 answerResList)
      exam          = Exam title passPercentage <$> questions
      answerViews   = zipWith3 (\x y z -> zip3 x y z) (chunksOf 4 aTextViews) (chunksOf 4 aBoolViews) (cycle [[1..4::Int]])
      questionViews = zip3 qTextViews answerViews ([1..]::[Int])
      widget        = [whamlet|
          #{token}
              <table class=questCreator>
                  $forall (qView, aList,n) <- questionViews
                          <tr>
                              <td class=questionTD>_{Question} _{Nr}. #{show n}:
                              <td style="color:black"> ^{fvInput qView}
                      $forall (tview, bview, c) <- aList
                          <tr>
                              <td class=smallWhite>_{Answer} _{Nr}.#{show c}:
                              <td style="color:black;"> ^{fvInput tview}
                              <td class=smallWhite>{IsCorrect}
                              <td class=smallWhite>^{fvInput bview}
                              <td>
                  <tr>
                  <td style="text-align:right;"><input type=submit value=_{MsgSubmitQuest questCount}>
                      |]
  return (exam, widget)

examAttributesForm :: Html -> MForm Handler ((FormResult ExamAttributes), Widget)
examAttributesForm token = do
    (eTitleResult, eTitleView) <- mreq textField "" Nothing
    (ePassResult, ePassView)   <- mreq unsignedDoubleField "" (Just 50.0)
    (eCountResult, eCountView) <- mreq unsignedIntField "" (Just 5)
    let examAttributes = ExamAttributes <$> eTitleResult <*> ePassResult <*> eCountResult
        widget = [whamlet|
        #{token}
            <table class=questCreator>
                <tr>
                    <td class=smallWhite> _{ExamTitle}:
                    <td style="color:black"> ^{fvInput eTitleView}
                <tr>
                    <td class=smallWhite> _{PassPercentage}:
                    <td span style="color:black"> ^{fvInput ePassView}
                    <td class=smallWhite style="text-align:left;"> %
                <tr>
                    <td class=smallWhite> _{QuestionNum}:
                    <td span style="color:black"> ^{fvInput eCountView}
                <tr>
                    <td>
                    <td style="text-align:right;"><input type=submit value=_{StartExam}>
                 |]
    return (examAttributes, widget)


toExamAttributes :: Text -> ExamAttributes
toExamAttributes ((splitOn "($)") -> [a, b, c])     =  let (Just passPercentage) = maybeDouble $ unpack b
                                                           (Just questCount)     = maybeInt $ unpack c in
                                                       ExamAttributes a passPercentage questCount
toExamAttributes _                                  = ExamAttributes (pack "Error in reading exam cookie") 0.0 0


