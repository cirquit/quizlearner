module Assets where

import Import
import Prelude (reads)

-- | Custom Fields

titleTextField ::( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Text
titleTextField msg = checkBool (\x -> ((length x) <= 200) && isNotSpaceList x) msg textField
  where isNotSpaceList l = not $ all (==' ') l

checkTextField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => Text -> msg -> Field m Text
checkTextField text msg = checkBool (==text) msg textField

unsignedIntField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Int
unsignedIntField msg = checkBool (>0) msg intField


unsignedProcentField :: ( RenderMessage (HandlerSite m) FormMessage
                       , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Double
unsignedProcentField msg = checkBool (\x -> x > 0 && x <= 100) msg doubleField

-- | DB

exampleDB :: MonadIO m => ReaderT SqlBackend m ()
exampleDB = do
    _ <- insert $ exam_1
    _ <- insert $ exam_2
    _ <- insert $ exam_3
    liftIO $ putStrLn "exampleDB was called"

-- | Helper

zipAnswers :: [Answer] -> [(Text, Int)]
zipAnswers xs = zipWith (\(Answer content _) n -> (content,n)) xs [0..]

toDouble :: (Integral a) => a -> Double
toDouble = fromIntegral

floor' :: (RealFrac a) => a -> Integer
floor' = floor

maybeRead :: Read a => String -> Maybe a
maybeRead (reads -> [(x,"")]) = Just x
maybeRead _                   = Nothing

maybeInt :: String -> Maybe Int
maybeInt = maybeRead

maybeDouble :: String -> Maybe Double
maybeDouble = maybeRead

encodeExamAttributes :: Text -> Double -> Int -> Text
encodeExamAttributes a b c = intercalate "($)" xs
  where
    xs = [a, ps b, ps c]
    ps = pack . show




































-- | Temporary Exams

exam_list :: [Exam]
exam_list =[exam_1, exam_2, exam_3]

exam_1, exam_2, exam_3 :: Exam
exam_1 = Exam {examTitle="Template 1", examPassPercentage=0.45, examQuestions=[q1,q2,q3,q4,q5,q6,q7,q8,q9,q6,q6,q6]}
exam_2 = Exam {examTitle="Template 2",  examPassPercentage=0.3, examQuestions=[q1,q2,q3]}
exam_3 = Exam {examTitle = "Intermediate Haskell", examPassPercentage = 0.8, examQuestions = [Question {questionContent = "Was ist der allgemeinste Typ von (compare . fst)?", questionAnswerList = [Answer {answerContent = "Eq b => (a, b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord b => (b, b1) -> b -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => (a,b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "a -> b -> Ordering ", answerIsCorrect = False}]},Question {questionContent = "Zu was ist zipWith (++) [['h','a','l','l','o']] ['w':'e':'l':'t':[]] \228quivalent?", questionAnswerList = [Answer {answerContent = "[\"hallo\", \"welt\"]", answerIsCorrect = False},Answer {answerContent = "\"hallowelt\"", answerIsCorrect = False},Answer {answerContent = "(\"hallo\" ++ \"welt\"):[]", answerIsCorrect = True},Answer {answerContent = "[\"hallowelt\"]", answerIsCorrect = True}]},Question {questionContent = "Welchen Typ hat foldl compare?", questionAnswerList = [Answer {answerContent = "Ordering -> [Ordering] -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => a -> [a] -> a", answerIsCorrect = False},Answer {answerContent = "[Ordering] -> Ordering -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord a => a -> [a] -> Ordering", answerIsCorrect = False}]},Question {questionContent = "Was ist der allgemeinste Typ von Just $ Just Nothing?", questionAnswerList = [Answer {answerContent = "a", answerIsCorrect = False},Answer {answerContent = "Maybe a", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe a)", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe (Maybe a))", answerIsCorrect = True}]},Question {questionContent = "Zu was ist scanl (+) 0 [1..10] \228quivalent?", questionAnswerList = [Answer {answerContent = "foldl (\\x y -> x++[sum y]) [] (inits [1..10])", answerIsCorrect = True},Answer {answerContent = "take 11 $ unfoldr (\\(x,y) -> Just (x,(x+y,y+1))) (0,1)", answerIsCorrect = True},Answer {answerContent = "[0,1,3,6,10,15,21,28,36,45,55]", answerIsCorrect = True},Answer {answerContent = "map sum $ inits [1..10]", answerIsCorrect = True}]},Question {questionContent = "Welchen allgemeinen Typ hat (*) 1.6 ?", questionAnswerList = [Answer {answerContent = "Num b => b -> b", answerIsCorrect = False},Answer {answerContent = "Integral a => a -> a", answerIsCorrect = False},Answer {answerContent = "Fractional a => a -> a", answerIsCorrect = True},Answer {answerContent = "Double -> Double", answerIsCorrect = False}]}]}

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