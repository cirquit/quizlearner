module Handler.Exam where

import Assets (zipAnswers, toDouble, roundByTwo, shuffle, getAllExams, isAuthor)
import Data.List ((!!), unzip, (\\), sortBy, repeat)
import Import hiding (unzip, (\\), sortBy, repeat)
import Widgets (titleWidget, iconWidget, publicExamWidget, postWidget,
                errorWidget, spacingScript, privateExamWidget)

-- | Look up and display exam
getExamR :: ExamId -> Handler Html
getExamR examId = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams, exam) <- runDB $ do
        (publicExams, privateExams)   <- getAllExams memail
        exam                          <- get404 examId
        return (publicExams, privateExams, exam)
    (widget, enctype) <- generateFormPost $ examMForm $ examQuestions exam

    case (examAuthor exam, memail) of
        (Nothing, _)        -> do let middleWidget = postWidget enctype widget
                                  defaultLayout $ do
                                      addScript $ StaticR js_nextBackScript_js
                                      $(widgetFile "exam")
        (isAuthor -> True) ->  do let middleWidget = postWidget enctype widget
                                  defaultLayout $ do
                                      addScript $ StaticR js_nextBackScript_js
                                      $(widgetFile "exam")
        (_,_)             -> redirect AccManagerR

-- | Evaluates exam and displays results
postExamR :: ExamId -> Handler Html
postExamR examId = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams, exam) <- runDB $ do
        (publicExams, privateExams)   <- getAllExams memail
        exam                          <- get404 examId
        return (publicExams, privateExams, exam)
    ((res,_), _) <- runFormPost $ examMForm $ examQuestions exam
    let middleWidget = case res of
         (FormSuccess list) -> let enumAnswers  = zip ([0..]::[Int]) list
                                   accPoints    = toDouble (calculateAccPoints enumAnswers exam)
                                   accPercent   = accPoints / toDouble ((4*) $ length $ examQuestions exam)
                                   roundPercent = roundByTwo accPercent
                                   passed       = accPercent >= examPassPercentage exam in
                                   [whamlet|
            ^{tableWidget enumAnswers exam}
                <p class=boldWhite> #{show accPoints}p | #{show roundPercent}%
            $if passed
                <p class=green> _{MsgPassExam $ examTitle exam}
            $else
                <p class=sadred> _{MsgNoPassExam $ examTitle exam}
            <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                                   |]
         (_)                -> [whamlet| ^{errorWidget $ pack "exam evaluation"}|]
    defaultLayout $(widgetFile "exam")

-- | Generates tabbed exam input form
examMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Int])]), Widget)
examMForm xs token  = do
    let checkBoxes  = checkboxesField' . optionsPairs . zipAnswers
        questionStr = fromString . unpack
    questionFields <- forM xs (\(Question content list ) -> mopt (checkBoxes list) (questionStr content) Nothing)
    let (questResults, questViews) = unzip questionFields
    shuffledViews <- liftIO $ shuffle questViews
    let numeratedViews = zip ([1..]::[Int]) shuffledViews
        widget = [whamlet|
            #{token}
                <ul class="tabs">
                    $forall (c,view) <- numeratedViews
                        <li>
                            <input type="radio" name="tabs" id="tab#{fvId view}">
                            <label for="tab#{fvId view}">#{show c}
                            <div id="tab-content#{fvId view}" class="tab-content animated fadeIn">
                                <p class=boldWhite> #{fvLabel view}
                                ^{fvInput view}
                                <br>
                <input class="evalButton" id="submitId" type=submit value=_{MsgEvaluate}>
                 |]
    return ((FormSuccess questResults), widget)

-- | Custom stylized checkbox field with random order
checkboxesField' :: (Eq a, RenderMessage site FormMessage) =>
                    HandlerT site IO (OptionList a) -> Field (HandlerT site IO) [a]
checkboxesField' ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val _ -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            shuffledOpts <-liftIO $ shuffle opts
            [whamlet|
    <span ##{theId}>
        $forall opt <- shuffledOpts
            <div>
                <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                <span class=simpleWhite> #{optionDisplay opt}

            |]
    }

-- | Exam evaluation
--   Evaluating the checked results
--   Sets True for given indices, False otherwise
--   [1,3] -> [False, True, False, True]
toBoolList :: [Int] -> [Bool]
toBoolList xs = map snd $ sortBy (comparing fst) $ ts ++ fs
    where ys = [0..3] \\ xs
          ts = zip xs (repeat True)
          fs = zip ys (repeat False)

-- | Compares solution with user answers for a single question
--   Score can't drop below 0 points
compareAnswers :: [Bool] -> Maybe [Bool] -> Int
compareAnswers xs gs = max 0 $ foldl' (\acc (x,y) -> if x == y then acc + 1 else acc - 1) 0 fs
  where fs = case gs of
                 (Just list) -> zip xs list
                 Nothing     -> zip xs $ repeat False

-- | Creates solution list for n-th question
getAnswers :: Exam -> Int -> [Bool]
getAnswers exam n = map (answerIsCorrect) qas
    where qas = questionAnswerList ((examQuestions exam) !! n)

-- | Calculates accumulated points
calculateAccPoints :: [(Int, FormResult (Maybe [Int]))] -> Exam -> Int
calculateAccPoints [] _  = 0
calculateAccPoints ((c,FormSuccess (Just xs)):ys) exam = compareAnswers (getAnswers exam c) (Just $ toBoolList xs) + calculateAccPoints ys exam
calculateAccPoints ((c,_):ys) exam                     = compareAnswers (getAnswers exam c) Nothing + calculateAccPoints ys exam

-- | Displays evaluated exam
tableWidget :: [(Int, FormResult (Maybe [Int]))] -> Exam -> Widget
tableWidget maybeAnswers exam = [whamlet|
    <table class=evalTable>
        <tr>
            <th> _{MsgQuestion 1}
            <th colspan="4"> _{MsgAnswer 4}
            <th> _{MsgPoints}
        $forall (c,(FormSuccess may)) <- maybeAnswers
            ^{evalQuestWidget exam c may}
                                |]

-- | Generates tooltips and displays checked answers
squareWidget :: [Bool] -> Int -> [Answer] -> Widget
squareWidget myAnswerL aIndex correctAnswerL = let square = if myAnswerL !! aIndex then [whamlet|☒|]
                                                                                   else [whamlet|☐|]
                                                   answer = correctAnswerL !! aIndex in
                                               [whamlet|
                                                   <th class=tooltips> ^{square}
                                                       <span> #{answerContent answer}
                                               |]

-- | Displays evaluated question
evalQuestWidget :: Exam -> Int -> Maybe [Int] -> Widget
evalQuestWidget exam qIndex maybeAnswers  = let correctResult     = getAnswers exam qIndex
                                                question          = (examQuestions exam) !! qIndex
                                                answerList        = questionAnswerList question
                                                falseL            = [False, False, False, False]
                                                wid results index = squareWidget results index answerList in
                                           [whamlet|
                                               <tr>
                                                         <th rowspan="2" class=tooltips> _{MsgNr}. #{show $ qIndex + 1}
                                                             <span> #{questionContent question}
                                                 $maybe just <- maybeAnswers
                                                     $with myResults <- toBoolList just
                                                         ^{wid myResults 0} ^{wid myResults 1} ^{wid myResults 2} ^{wid myResults 3}
                                                         <th rowspan="2"> #{show $ compareAnswers correctResult (Just myResults)}
                                                 $nothing
                                                         ^{wid falseL 0} ^{wid falseL 1} ^{wid falseL 2} ^{wid falseL 3}
                                                         <th rowspan="2"> #{show $ compareAnswers correctResult Nothing}
                                               <tr style="background-color:#31914E;">
                                                         ^{wid correctResult 0} ^{wid correctResult 1} ^{wid correctResult 2} ^{wid correctResult 3}
                                           |]