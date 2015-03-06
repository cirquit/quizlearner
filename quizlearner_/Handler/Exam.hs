module Handler.Exam where

import Assets (zipAnswers, toDouble, floor')
import Widgets (titleWidget, iconWidget, leftWidget, postWidget, errorWidget, spacingScript)
import Import hiding (unzip, (\\), sortBy, repeat)
import Data.List ((!!), unzip, (\\), sortBy, repeat)

getExamR :: ExamId -> Handler Html
getExamR examId = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    exam <-  runDB $ get404 examId
    (widget, enctype) <-generateFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = postWidget enctype widget
    defaultLayout $ do $(widgetFile "exam")

postExamR :: ExamId -> Handler Html
postExamR examId = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    exam  <- runDB $ get404 examId
    ((res,_), _) <- runFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = case res of
         (FormSuccess list) -> let newList = zip ([0..]::[Int]) list
                                   accPoints  = toDouble (calculatePoints newList exam)
                                   accPercent = accPoints / toDouble ((4*) $ length $ examQuestions  exam)
                                   passed = accPercent >= examPassPercentage exam
                                   roundPercent = (toDouble  $ floor' $ accPercent * 10000) / 100 in
                                   [whamlet|
     ^{tableWidget newList exam}
         <p class=boldWhite> #{show accPoints}p | #{show roundPercent}%
     $if passed
         <p class=green> _{MsgPassedExam}
     $else
         <p class=sadred> _{MsgNotPassExam}
     <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> _{MsgGetBack} </label>
                                   |]
         _                  -> [whamlet|
     ^{errorWidget $ pack "Evaluation"}
                               |]
    defaultLayout $ do $(widgetFile "exam")

listEditMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Int])]), Widget)
listEditMForm xs token = do
    let checkBoxes = checkboxesFieldList' . zipAnswers
    let questionStr = fromString . unpack
    checkFields <- forM xs (\(Question content list ) -> mopt (checkBoxes list) (questionStr content) Nothing)
    let (checkResults, checkViews) = unzip checkFields
    let numeratedViews = zip ([1..]::[Int]) checkViews
    let widget = [whamlet|
        #{token}
            <ul class="tabs">
                $forall (c,view) <- numeratedViews
                    <li>
                        <input type="radio" name="tabs" id="tab#{fvId view}">
                        <label for="tab#{fvId view}">_{MsgQ} #{show c}
                        <div id="tab-content#{fvId view}" class="tab-content animated fadeIn">
                            <p class=boldWhite> #{fvLabel view} </p>
                            ^{fvInput view}
                            <br>
            <input class=button type=submit value=_{MsgEvaluate}>
                 |]
    return ((FormSuccess checkResults), widget)

checkboxesFieldList' :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg) =>
                              [(msg, a)] -> Field (HandlerT site IO) [a]
checkboxesFieldList' = checkboxesField' . optionsPairs

checkboxesField' :: (Eq a, RenderMessage site FormMessage) =>
                    HandlerT site IO (OptionList a) -> Field (HandlerT site IO) [a]
checkboxesField' ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val _ -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            [whamlet|
    <span ##{theId}>
        $forall opt <- opts
            <div>
                <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                <span class=simpleWhite> #{optionDisplay opt}

            |]
    }

-- | Exam evaluation

-- | Evaluating the checked results
-- | [1,3] -> [False, True, False, True]
toBoolList :: [Int] -> [Bool]
toBoolList xs = map snd $ sortBy (comparing fst) $ ts ++ fs
    where
        ys = [0..3] \\ xs
        ts = zip xs (repeat True)
        fs = zip ys (repeat False)

compareAnswers :: [Bool] -> Maybe [Bool] -> Int
compareAnswers xs (Just zs) = max 0 $ foldl' (\ys (x,y) -> if x == y then ys + 1 else ys - 1) 0 (zip xs zs)
compareAnswers xs Nothing   = max 0 $ foldl' (\ys (x,y) -> if x == y then ys + 1 else ys - 1) 0 (zip xs fs)
  where fs = [False, False, False, False]

getAnswers :: Exam -> Int -> [Bool]
getAnswers exam n = map (answerIsCorrect) qas
    where qas = questionAnswerList ((examQuestions exam) !! n)

calculatePoints :: [(Int, FormResult (Maybe [Int]))] -> Exam -> Int
calculatePoints [] _  = 0
calculatePoints ((c,(FormSuccess (Just xs))):ys) exam = compareAnswers (getAnswers exam c) (Just $ toBoolList xs) + calculatePoints ys exam
calculatePoints ((c,(_)):ys) exam = compareAnswers (getAnswers exam c) Nothing + calculatePoints ys exam

tableWidget :: [(Int, FormResult (Maybe [Int]))] -> Exam -> Widget
tableWidget maybeAnswers exam = [whamlet|
    <table class=evalTable>
        <tr>
            <th> _{MsgQuestion}
            <th colspan="4"> _{Answer 4}
            <th> _{MsgPoints}
        $forall (c,(FormSuccess may)) <- maybeAnswers
            ^{evalWidget exam c may}
                                |]

squareWidget :: [Bool] -> Int -> [Answer] -> Widget
squareWidget myAnswerL aIndex correctAnswerL = let square = if myAnswerL !! aIndex then [whamlet|☒|]
                                                                                   else [whamlet|☐|]
                                                   answer = correctAnswerL !! aIndex in
                                               [whamlet|
                                                   <th class=tooltips>^{square}
                                                       <span> #{answerContent answer}
                                               |]

evalWidget :: Exam -> Int -> Maybe [Int] -> Widget
evalWidget exam qIndex maybeAnswers  = let correctResult     = getAnswers exam qIndex
                                           question          = (examQuestions exam) !! qIndex
                                           answerList        = questionAnswerList question
                                           falseL            = [False, False, False, False]
                                           wid results index = squareWidget results index answerList in
                                           [whamlet|
                                               <tr>
                                                         <th rowspan="2" class=tooltips> _{Nr}. #{show qIndex}
                                                             <span> #{questionContent question}
                                                 $maybe just <- maybeAnswers
                                                     $with myResults <- toBoolList just
                                                         ^{wid myResults 0} ^{wid myResults 1} ^{wid myResults 2} ^{wid myResults 3}
                                                         <th rowspan="2"> #{show $ compareAnswers correctResult (Just myResults)}p
                                                 $nothing
                                                         ^{wid falseL 0} ^{wid falseL 1} ^{wid falseL 2} ^{wid falseL 3}
                                                         <th rowspan="2"> #{show $ compareAnswers correctResult Nothing}p
                                               <tr style="background-color:#31914E;">
                                                         ^{wid correctResult 0} ^{wid correctResult 1} ^{wid correctResult 2} ^{wid correctResult 3}
                                           |]