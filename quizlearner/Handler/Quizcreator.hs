module Handler.Quizcreator where

import Assets (unsignedProcentField, unsignedIntField,  maybeInt,
               maybeDouble, encodeExamAttributes, titleTextField,
               noSpacesTextField, getAllExams)
import Data.Text (splitOn)
import Data.List.Split (chunksOf)
import Data.List (cycle)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, postWidget,
                errorWidget, spacingScript, privateExamWidget)

-- | Checks if exam attributes have already been submitted
--   If so, proceed to question input
--   If not, let the user submit them
getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    mayAttributes  <- lookupSession "examAttributes"
    let generatePost = case mayAttributes of
                           (Just text) -> generateFormPost $ examForm $ toExamAttributes text
                           _           -> generateFormPost $ examAttributesMForm
    (widget, enctype) <- generatePost
    let middleWidget = postWidget enctype widget
    defaultLayout $(widgetFile "quizcreator")

-- | Checks if exam attributes have already been submitted
--   If so, checks if questions have been submitted correctly and saves the exam in that case (shows error otherwise)
--   If not, submits entered input and redirects to question input
postQuizcreatorR :: Handler Html
postQuizcreatorR = do
    mayAttributes  <- lookupSession "examAttributes"
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    case mayAttributes of
        (Just text) -> do ((res, _), _) <- runFormPost $ examForm $ toExamAttributes text
                          case res of
                                  (FormSuccess exam) -> do
                                      deleteSession "examAttributes"
                                      _ <- runDB $ insert $ exam memail
                                      redirect QuizcreatorR
                                  (_) -> do
                                      let middleWidget = errorWidget "Exam parsing"
                                      defaultLayout $ do $(widgetFile "quizcreator")
        (_)         -> do ((res, _), _) <- runFormPost examAttributesMForm
                          case res of
                                  (FormSuccess (ExamAttributes title percent qCount)) -> do
                                      setSession "examAttributes" $ encodeExamAttributes title percent qCount
                                      redirect QuizcreatorR
                                  (_) -> do
                                      deleteSession "examAttributes"
                                      redirect QuizcreatorR

-- | Generates enumerated exam form based on previously submitted exam attributes
examForm :: ExamAttributes -> Html -> MForm Handler ((FormResult (Maybe Text -> Exam)), Widget)
examForm (ExamAttributes title passPercentage questCount) token = do
    qTextFields <- replicateM questCount (mreq noSpacesTextField "" Nothing)
    aTextFields <- replicateM (4 * questCount) (mreq noSpacesTextField "" Nothing)
    aBoolFields <- replicateM (4 * questCount) (mreq boolField "" (Just False))
    let (qTextResults, qTextViews) = unzip qTextFields
        (aTextResults, aTextViews) = unzip aTextFields
        (aBoolResults, aBoolViews) = unzip aBoolFields
        answerResList = zipWith (\(FormSuccess x) (FormSuccess y)  -> Answer x y) aTextResults aBoolResults
        questions     = FormSuccess $ zipWith (\(FormSuccess q) as -> Question q as) qTextResults (chunksOf 4 answerResList)
        exam          = Exam title passPercentage <$> questions
        answerViews   = zipWith3 zip3 (chunksOf 4 aTextViews) (chunksOf 4 aBoolViews) (cycle [[1..4::Int]])
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

-- | Generates exam attributes form
examAttributesMForm :: Html -> MForm Handler ((FormResult ExamAttributes), Widget)
examAttributesMForm token = do
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

-- | Reads exam attributes from cookie
toExamAttributes :: Text -> ExamAttributes
toExamAttributes ((splitOn "($)") -> [a, b, c]) = let (Just passPercentage) = maybeDouble $ unpack b
                                                      (Just questCount)     = maybeInt $ unpack c
                                                      title = (unwords . words) a in
                                                  ExamAttributes title passPercentage questCount
toExamAttributes _                              = ExamAttributes msg 0.0 0
  where msg = "Error in cookie" :: Text


data ExamAttributes = ExamAttributes {
                    title    :: Text
                  , passPercentage :: Double
                  , questCount :: Int
                }



