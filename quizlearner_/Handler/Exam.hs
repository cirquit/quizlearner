module Handler.Exam where

import Assets (zipAnswers, titleWidget, iconWidget, leftWidget, toDouble, floor')
import Import hiding (unzip)
import Data.List ((!!), unzip)

getExamR :: ExamId -> Handler Html
getExamR examId = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    exam <-  runDB $ get404 examId
    (widget, enctype) <-generateFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = [whamlet|
                           <form method=post enctype=#{enctype}>
                               ^{widget}
                       |]
    defaultLayout $ do $(widgetFile "exam")

postExamR :: ExamId -> Handler Html
postExamR examId = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    exam  <- runDB $ get404 examId
    ((res,_), _) <- runFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = case res of
         (FormSuccess list) -> let newList = zip ([0..]::[Int]) list
                                   accPoints  = toDouble (accumAnswers newList exam)
                                   accPercent = accPoints / toDouble (examMaxScore exam)
                                   passed = accPercent >= examPassPercentage exam
                                   roundPercent = (toDouble  $ floor' $ accPercent * 10000) / 100 in
                                   [whamlet|
                                    ^{tableWidget newList exam}
                                        <p class=boldWhite> #{show accPoints}p | #{show roundPercent}%
                                    $if passed
                                        <p class=green> Congratulations, you passed the test!
                                    $else
                                        <p class=sadred> Sorry, you didn't pass.
                                    <a href=@{HomeR} style="margin:10px;"> <label class=simpleOrange> Get back! </label>
                                   |]
         _                  -> [whamlet|
                                   <span class=boldWhite> Oops! Something went wrong!
                                   <a href=@{HomeR} style="margin:10px"> <label class=simpleOrange> Get back! </label>
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
                                         <label for="tab#{fvId view}">Q #{show c}
                                         <div id="tab-content#{fvId view}" class="tab-content animated fadeIn">
                                             <p class=boldWhite> #{fvLabel view}: </p>
                                             ^{fvInput view}
                                             <br>
                     <input class=button type=submit value="Testing">
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
                        <label>
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                            <span class=simpleWhite> #{optionDisplay opt}
            |]
    }

spacingScript :: Exam -> Widget
spacingScript exam = toWidget [hamlet|
                                 <script>
                                     var divList = document.getElementsByClassName("tab-content");
                                         for(i=0; i<divList.length; i++){
                                             divList[i].style.top = 40*Math.ceil(#{length $ examQuestions exam}/10) + "px";
                                         }
                             |]

toBoolList :: [Int] -> [Bool]
toBoolList xs = snd $ unzip $ acc xs [(0, False), (1, False), (2,False), (3,False)]
  where acc :: [Int] -> [(Int, Bool)] -> [(Int, Bool)]
        acc []     bs = bs
        acc (z:zs) bs = acc zs (foldr (\(n,y) ys -> if n == z then (n,True):ys else (n,y):ys) [] bs)

compareAnswers :: [Bool] -> Maybe [Bool] -> Int
compareAnswers xs (Just zs) = foldl' (\ys (x,y) -> if x == y then ys + 1 else ys) 0 (zip xs zs)
compareAnswers xs Nothing   = foldl' (\ys (x,y) -> if x == y then ys + 1 else ys) 0 (zip xs fs)
  where fs = [False, False, False, False]

getAnswers :: Exam -> Int -> [Bool]
getAnswers exam n = map (answerIsCorrect) qas
    where qas = questionAnswerList ((examQuestions exam) !! n)

accumAnswers :: [(Int, FormResult (Maybe [Int]))] -> Exam -> Int
accumAnswers [] _  = 0
accumAnswers ((c,(FormSuccess (Just xs))):ys) exam = compareAnswers (getAnswers exam c) (Just $ toBoolList xs) + accumAnswers ys exam
accumAnswers ((c,(_)):ys) exam = compareAnswers (getAnswers exam c) Nothing + accumAnswers ys exam

tableWidget :: (MonadThrow m, MonadBaseControl IO m, MonadIO m, Foldable t) =>
        t (Int, FormResult (Maybe [Int])) -> Exam -> WidgetT App m ()
tableWidget maybeAnswers exam = [whamlet|
                                    <table class=evalTable>
                                        <tr>
                                            <th> Question
                                            <th colspan="4"> Answers
                                            <th> Points
                                        $forall (c,(FormSuccess may)) <- maybeAnswers
                                            ^{evalWidget exam c may}
                                |]

squareWidget :: [Bool] --my answers
             -> Int     --answer index
             -> [Answer]
             -> Widget
squareWidget list aIndex answerL = let square = if list !! aIndex then [whamlet|☒|]
                                                                  else [whamlet|☐|]
                                       answer = answerL !! aIndex in
                                              [whamlet|
                                                  <th class=tooltips>^{square}
                                                      <span> #{answerContent answer}
                                              |]

evalWidget :: Exam
            -> Int         -- question index
            -> Maybe [Int] -- my possible answers
            -> Widget
evalWidget exam qIndex maybeAnswers  = let correctResult     = getAnswers exam qIndex
                                           question          = (examQuestions exam) !! qIndex
                                           answerList        = questionAnswerList question
                                           falseL            = [False, False, False, False]
                                           wid results index = squareWidget results index answerList in
                                           [whamlet|
                                               <tr>
                                                         <th rowspan="2" class=tooltips> Nr. #{show qIndex}
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