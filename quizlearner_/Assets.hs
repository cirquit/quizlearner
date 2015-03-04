module Assets where

import Import hiding (images_quizCreator_svg, images_uploadXML_svg, images_xml_svg)
import Prelude (reads)


-- | Static Files

staticFiles "static"

iconWidget :: Widget
iconWidget = do
             toWidget [hamlet| <a href=@{QuizcreatorR}>
                                   <img src=@{StaticR images_quizCreator_svg} id="quiz_creator" title=#{q_creator_title} width="80px" height="80px">
                               <a href=@{UploadR}>
                                   <img src=@{StaticR images_uploadXML_svg} id="upload_xml" title=#{upload_xml_title} width="80px" height="80px">
                      |]
             toWidget [lucius| #quiz_creator, #upload_xml {float: right; margin: 30px;}
                      |]

q_creator_title :: Text
q_creator_title = pack "Click this to create a new exam"

upload_xml_title :: Text
upload_xml_title = pack "Click this to upload an XML file"

-- | Widgets

createExam :: Exam -> Widget
createExam  (Exam title maxScore maxTime passPercentage qList) = [whamlet|
    <p class=simpleWhite> Examtitle: #{title}
    <p class=simpleWhite> MaxScore: #{show maxScore}
    <p class=simpleWhite> MaxTime: #{show maxTime}
    <p class=simpleWhite> PassPercentage: #{show passPercentage}
    $forall (Question content aList) <- qList
        <p class=simpleWhite> Question: #{content}
            $forall (Answer aContent isCorrect) <- aList
                 <p class=simpleWhite> Answer: #{aContent} #{show isCorrect}
                                                                  |]

postWidget :: Enctype -> Widget -> Widget
postWidget enctype widget =  [whamlet|
    <form method=post enctype=#{enctype}>
        ^{widget}
                             |]

titleWidget :: Widget
titleWidget = toWidget [hamlet|
    <a class=mainTitle href=@{HomeR}>
        <span style="color:#FAA500;">Quiz</span>Learner<br>
                       |]

leftWidget :: [Entity Exam] -> Widget
leftWidget exams = toWidget [hamlet|
    <p class=orangeTitle> [Exams]
    <ul class=examList style="padding-left: 0px;">
          $if null exams
                  <p class=sadred> Couldn't find any exams in the DB! Please refresh!
          $else
              $forall (Entity examId exam) <- exams
                  <li class=examList>
                    <a href=@{ExamR examId}> #{examTitle exam} </a>
                    <a href=@{XmlR examId}> <img src=@{StaticR images_xml_svg} title="Show XML" height="20px"> </a>
                    <a href=@{DeleteR examId}> <img src=@{StaticR images_trashcan_svg} title="Delete exam" height="25px"> </a>
                            |]

errorWidget  :: Text -> Widget
errorWidget text = [whamlet|
    <span class=simpleWhite> Something went wrong...in #{text}
         <a href=@{HomeR} style="margin:10px"> <label class=simpleOrange> Get back! </label>
|]

spacingScript :: Widget
spacingScript = toWidget [hamlet|
    <script>
        var divList = document.getElementsByClassName("tab-content");
            for(i=0; i<divList.length; i++){
                divList[i].style.top = 40*Math.ceil(divList.length/10) + "px";
            }
                             |]

-- | Custom Fields

checkTextField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Text -> Field m Text
checkTextField text = checkBool (==text) msg textField
        where msg = "The title is not correct, please try again" :: Text


unsignedIntField:: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Int
unsignedIntField = checkBool (>0) msg intField
  where msg = "This input can't be negative" :: Text

unsignedDoubleField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Double
unsignedDoubleField = checkBool (>0) msg doubleField
  where msg = "This input can't be negative" :: Text

-- | DB

loadDB :: MonadIO m => ReaderT SqlBackend m ()
loadDB = do
    _ <- insert $ exam_1
    _ <- insert $ exam_2
    _ <- insert $ exam_3
    liftIO $ putStrLn "The function load_DB was called!"

-- | Helper

cookie_to_textbool :: Char -> Text
cookie_to_textbool x = if x == '1' then pack "True"
                                   else pack "False"

bool_to_cookie :: [Bool] -> Text
bool_to_cookie l = pack $ foldr (\x xs -> (bool_to_char x) : xs) "" l
  where bool_to_char :: Bool -> Char
        bool_to_char x = if x then '1' else '0'

show_right_answers :: Question -> Text
show_right_answers quest = pack $ concatMap ((++ " ") . show . answerIsCorrect) (questionAnswerList quest)

zipAnswers :: [Answer] -> [(Text, Int)]
zipAnswers [] = []
zipAnswers xs = acc 0 xs
     where  acc _ []  = []
            acc n ((Answer text _ ):xs') =  (text, n):acc (n+1) xs'


toDouble :: (Integral a) => a -> Double
toDouble = fromIntegral

floor' :: (RealFrac a) => a -> Integer
floor' = floor

maybeRead :: Read a => String -> Maybe a
maybeRead (reads -> [(x,"")]) = Just x
maybeRead _ = Nothing

maybeInt :: String -> Maybe Int
maybeInt = maybeRead

maybeDouble :: String -> Maybe Double
maybeDouble = maybeRead

encodeExamAttributes :: Text -> Int -> Int -> Double -> Int -> Text
encodeExamAttributes a b c d e = a ++ pack (' ':show b) ++ pack (' ':show c) ++ pack (' ':show d) ++ pack (' ':show e)




































-- ###################################################################################
-- Temporary Exams
exam_3 :: Exam
exam_3 = Exam {examTitle = "Haskell Basics", examMaxScore = 24, examMaxTime = 90, examPassPercentage = 0.8, examQuestions = [Question {questionContent = "Was ist der allgemeinste Typ von (compare . fst)?", questionAnswerList = [Answer {answerContent = "Eq b => (a, b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord b => (b, b1) -> b -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => (a,b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "a -> b -> Ordering ", answerIsCorrect = False}]},Question {questionContent = "Zu was ist zipWith (++) [['h','a','l','l','o']] ['w':'e':'l':'t':[]] \228quivalent?", questionAnswerList = [Answer {answerContent = "[\"hallo\", \"welt\"]", answerIsCorrect = False},Answer {answerContent = "\"hallowelt\"", answerIsCorrect = False},Answer {answerContent = "(\"hallo\" ++ \"welt\"):[]", answerIsCorrect = True},Answer {answerContent = "[\"hallowelt\"]", answerIsCorrect = True}]},Question {questionContent = "Welchen Typ hat foldl compare?", questionAnswerList = [Answer {answerContent = "Ordering -> [Ordering] -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => a -> [a] -> a", answerIsCorrect = False},Answer {answerContent = "[Ordering] -> Ordering -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord a => a -> [a] -> Ordering", answerIsCorrect = False}]},Question {questionContent = "Was ist der allgemeinste Typ von Just $ Just Nothing?", questionAnswerList = [Answer {answerContent = "a", answerIsCorrect = False},Answer {answerContent = "Maybe a", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe a)", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe (Maybe a))", answerIsCorrect = True}]},Question {questionContent = "Zu was ist scanl (+) 0 [1..10] \228quivalent?", questionAnswerList = [Answer {answerContent = "foldl (\\x y -> x++[sum y]) [] (inits [1..10])", answerIsCorrect = True},Answer {answerContent = "take 11 $ unfoldr (\\(x,y) -> Just (x,(x+y,y+1))) (0,1)", answerIsCorrect = True},Answer {answerContent = "[0,1,3,6,10,15,21,28,36,45,55]", answerIsCorrect = True},Answer {answerContent = "map sum $ inits [1..10]", answerIsCorrect = True}]},Question {questionContent = "Welchen allgemeinen Typ hat (*) 1.6 ?", questionAnswerList = [Answer {answerContent = "Num b => b -> b", answerIsCorrect = False},Answer {answerContent = "Integral a => a -> a", answerIsCorrect = False},Answer {answerContent = "Fractional a => a -> a", answerIsCorrect = True},Answer {answerContent = "Double -> Double", answerIsCorrect = False}]}]}


exam_list :: [Exam]
exam_list =[exam_1, exam_2, exam_3]

exam_1, exam_2 :: Exam
exam_1 = Exam {examTitle="Lineare Algebra", examMaxScore=50, examMaxTime=120, examPassPercentage=0.45, examQuestions=[q1,q2,q3,q4,q5,q6,q7,q8,q9,q6,q6,q6]}
exam_2 = Exam {examTitle="FFP",  examMaxScore=12, examMaxTime=180, examPassPercentage=0.3, examQuestions=[q1,q2,q3]}

-- Temporary Questions
q1,q2,q3,q4,q5,q6,q7,q8,q9 :: Question
q1 = Question {questionContent="Wieviel ist 2+3?", questionAnswerList=q1_answers}
q2 = Question {questionContent="Wieviel ist 3+4?", questionAnswerList=q2_answers}
q3 = Question {questionContent="Wieviel ist 4+5?", questionAnswerList=q3_answers}
q4 = Question {questionContent="Wieviel ist 2+3?", questionAnswerList=q4_answers}
q5 = Question {questionContent="Wieviel ist 3+4?", questionAnswerList=q5_answers}
q6 = Question {questionContent="Wieviel ist 4+5?", questionAnswerList=q6_answers}
q7 = Question {questionContent="Wieviel ist 2+3?", questionAnswerList=q7_answers}
q8 = Question {questionContent="Wieviel ist 3+4?", questionAnswerList=q8_answers}
q9 = Question {questionContent="Wieviel ist 4+5?", questionAnswerList=q9_answers}

-- Temporary Answers
q1_answers, q2_answers, q3_answers, q4_answers, q5_answers, q6_answers, q7_answers, q8_answers, q9_answers :: [Answer]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]
q2_answers = [q2_a1,q2_a2,q2_a3,q2_a4]
q3_answers = [q3_a1,q3_a2,q3_a3,q3_a4]
q4_answers = [q4_a1,q4_a2,q4_a3,q4_a4]
q5_answers = [q5_a1,q5_a2,q5_a3,q5_a4]
q6_answers = [q6_a1,q6_a2,q6_a3,q6_a4]
q7_answers = [q7_a1,q7_a2,q7_a3,q7_a4]
q8_answers = [q8_a1,q8_a2,q8_a3,q8_a4]
q9_answers = [q9_a1,q9_a2,q9_a3,q9_a4]


q1_a1, q1_a2, q1_a3, q1_a4, q2_a1, q2_a2, q2_a3, q2_a4, q3_a1, q3_a2, q3_a3, q3_a4 :: Answer
q4_a1, q4_a2, q4_a3, q4_a4, q5_a1, q5_a2, q5_a3, q5_a4, q6_a1, q6_a2, q6_a3, q6_a4 :: Answer
q7_a1, q7_a2, q7_a3, q7_a4, q8_a1, q8_a2, q8_a3, q8_a4, q9_a1, q9_a2, q9_a3, q9_a4 :: Answer

q1_a1 = Answer {answerContent="1", answerIsCorrect=False}
q1_a2 = Answer {answerContent="2", answerIsCorrect=False}
q1_a3 = Answer {answerContent="3", answerIsCorrect=False}
q1_a4 = Answer {answerContent="5", answerIsCorrect=True }
q2_a1 = Answer {answerContent="5", answerIsCorrect=False}
q2_a2 = Answer {answerContent="9", answerIsCorrect=False}
q2_a3 = Answer {answerContent="3", answerIsCorrect=False}
q2_a4 = Answer {answerContent="1", answerIsCorrect=True }
q3_a1 = Answer {answerContent="1", answerIsCorrect=False}
q3_a2 = Answer {answerContent="9", answerIsCorrect=False}
q3_a3 = Answer {answerContent="7", answerIsCorrect=False}
q3_a4 = Answer {answerContent="2", answerIsCorrect=True }
q4_a1 = Answer {answerContent="1", answerIsCorrect=False}
q4_a2 = Answer {answerContent="2", answerIsCorrect=False}
q4_a3 = Answer {answerContent="4", answerIsCorrect=False}
q4_a4 = Answer {answerContent="5", answerIsCorrect=True }
q5_a1 = Answer {answerContent="5", answerIsCorrect=False}
q5_a2 = Answer {answerContent="2", answerIsCorrect=False}
q5_a3 = Answer {answerContent="3", answerIsCorrect=False}
q5_a4 = Answer {answerContent="1", answerIsCorrect=True }
q6_a1 = Answer {answerContent="1", answerIsCorrect=False}
q6_a2 = Answer {answerContent="9", answerIsCorrect=False}
q6_a3 = Answer {answerContent="7", answerIsCorrect=False}
q6_a4 = Answer {answerContent="2", answerIsCorrect=True }
q7_a1 = Answer {answerContent="1", answerIsCorrect=False}
q7_a2 = Answer {answerContent="2", answerIsCorrect=False}
q7_a3 = Answer {answerContent="4", answerIsCorrect=False}
q7_a4 = Answer {answerContent="5", answerIsCorrect=True }
q8_a1 = Answer {answerContent="5", answerIsCorrect=False}
q8_a2 = Answer {answerContent="2", answerIsCorrect=False}
q8_a3 = Answer {answerContent="3", answerIsCorrect=False}
q8_a4 = Answer {answerContent="1", answerIsCorrect=True }
q9_a1 = Answer {answerContent="1", answerIsCorrect=False}
q9_a2 = Answer {answerContent="9", answerIsCorrect=False}
q9_a3 = Answer {answerContent="7", answerIsCorrect=False}
q9_a4 = Answer {answerContent="2", answerIsCorrect=True }


-- ######## DEPRICATED ###############
-- shuffle_answers :: [Answer] -> IO ([Answer])
-- shuffle_answers list = do
--     let n = L.length list
--     seed <- getStdRandom (randomR (0, (product [1..n]) - 1))
--     return $ permutations list !! seed
--
--     shuffled_answers <- liftIO $ shuffle_answers $ answer_list snd_q


-- examForm with labels (not really working - hamlet only)
--        #{token}
--          <ul class="tabs">
--              $forall (qView, aList,n) <- questionViews
--                  <li>
--                      <input type="radio" name="tabs" id="tab#{fvId qView}">
--                      <label for="tab#{fvId qView}">Q #{show n}
--                      <div id="tab-content#{fvId qView}" class="tab-content animated fadeIn">
--                          <p class=simpleWhite> ^{fvInput qView} </p>
--                              <span ##{fvId qView}>
--                                  $forall (tview, bview, c) <- aList
--                                     <div>
--                                         <span class=littleWhite>Answer Nr.#{show c} <span style="color:black"> ^{fvInput tview} </span> <span class=littleWhite>Is it correct? ^{fvInput bview} </span>
--            <input type=submit value="Submit question!">)