module Handler.Quizcreator where

import Import
import Assets (unsignedProcentField, unsignedIntField,  maybeInt,
               maybeDouble, encodeExamAttributes, titleTextField)
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
    setUltDestCurrent
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
                              <td class=questionTD>_{MsgQuestion 1} _{MsgNr}. #{show n}:
                              <td style="color:black"> ^{fvInput qView}
                      $forall (tview, bview, c) <- aList
                          <tr>
                              <td class=smallWhite>_{MsgAnswer 1} _{MsgNr}.#{show c}:
                              <td style="color:black;"> ^{fvInput tview}
                              <td class=smallWhite>_{MsgIsCorrect}
                              <td class=smallWhite>^{fvInput bview}
                              <td>
                  <tr>
                      <td style="text-align:left;">
                              <a class=questionTD href=@{SessionMasterR}> _{MsgGetBack}
                      <td>
                      <td colspan=2 style="text-align:right;"><input type=submit value="_{MsgSubmitQuest questCount}">
                      |]
  return (exam, widget)

examAttributesForm :: Html -> MForm Handler ((FormResult ExamAttributes), Widget)
examAttributesForm token = do
    (eTitleResult, eTitleView) <- mreq (titleTextField MsgExamTitle) "" Nothing
    (ePassResult, ePassView)   <- mreq (unsignedProcentField MsgInputNeg) "" (Just 50.0)
    (eCountResult, eCountView) <- mreq (unsignedIntField MsgInputNeg)"" (Just 5)
    let examAttributes = ExamAttributes <$> eTitleResult <*> ePassResult <*> eCountResult
        widget = [whamlet|
        #{token}
            <table class=questCreator>
                <tr>
                    <td class=smallWhite> _{MsgExamTitle}:
                    <td style="color:black"> ^{fvInput eTitleView}
                    <td class=smallWhite style="text-align:left;"> _{MsgErrExamTitle}
                <tr>
                    <td class=smallWhite> _{MsgPassPercentage}:
                    <td span style="color:black"> ^{fvInput ePassView}
                    <td class=smallWhite style="text-align:left;"> _{MsgErrPassPercentage}
                <tr>
                    <td class=smallWhite> _{MsgQuestionNum}:
                    <td span style="color:black"> ^{fvInput eCountView}
                    <td class=smallWhite style="text-align:left;"> _{MsgErrQuestionNum}
                <tr>
                    <td>
                    <td style="text-align:right;"><input type=submit value="_{MsgStartExam}">
                 |]
    return (examAttributes, widget)


toExamAttributes :: Text -> ExamAttributes
toExamAttributes ((splitOn "($)") -> [a, b, c])     = let (Just passPercentage) = maybeDouble $ unpack b
                                                          (Just questCount)     = maybeInt $ unpack c
                                                          title = (unwords . words) a in
                                                      ExamAttributes title passPercentage questCount
toExamAttributes _                                  = ExamAttributes msg 0.0 0
  where msg = "Error in cookie" :: Text -- MsgErrorCookie :: Text

