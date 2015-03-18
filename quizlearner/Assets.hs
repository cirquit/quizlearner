module Assets where

import Import
import Prelude (reads)
import System.Random
import Data.Array.IO

-- | Custom textfield that doesn't allow "spaces-only"-input or inputs longer than 200 characters
titleTextField ::( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Text
titleTextField msg = checkBool (\x -> ((length x) <= 200) && noSpaceList x) msg textField
    where noSpaceList l = not $ all (==' ') l

-- | Custom textfield that checks if entered value is equal to expected value
checkTextField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => Text -> msg -> Field m Text
checkTextField text msg = checkBool (==text) msg textField

-- | Custom Int field that doesn't allow number below 0
unsignedIntField :: ( RenderMessage (HandlerSite m) FormMessage
                   , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Int
unsignedIntField msg = checkBool (>0) msg intField

-- | Custom Int field that only allows numbers between 0 and 100 (inclusive)
unsignedProcentField :: ( RenderMessage (HandlerSite m) FormMessage
                       , RenderMessage (HandlerSite m) msg, Monad m )
               => msg -> Field m Double
unsignedProcentField msg = checkBool (\x -> x > 0 && x <= 100) msg doubleField

-- | Creates a input with javascript that trims spaces from both ends
noSpacesTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
noSpacesTextField = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
            $newline never
            <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}" onBlur="$('##{theId}').val($('##{theId}').val().trim());">
            |]
    , fieldEnctype = UrlEncoded
    }

-- | Loads default exams
exampleDB :: MonadIO m => ReaderT SqlBackend m ()
exampleDB = do
    _ <- insert $ exam_1
    _ <- insert $ exam_2
    liftIO $ putStrLn "exampleDB was called"

-- | Returns all public and private exams based on if your email
getAllExams :: MonadIO m => Maybe Text -> ReaderT SqlBackend m (([Entity Exam], [Entity Exam]))
getAllExams memail = do
        publicExams     <- selectList [ExamAuthor ==. Nothing] [Asc ExamTitle]
        privateExams    <- case memail of
                                  (Just _) -> selectList [ExamAuthor ==. memail] [Asc ExamTitle]
                                  (_)      -> return []
        return (publicExams, privateExams)

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
    where xs = [a, ps b, ps c]
          ps = pack . show

roundByTwo :: Double -> Double
roundByTwo n = (toDouble $ floor' $ n * 10000) / 100


-- | http://stackoverflow.com/questions/14692059/how-to-shuffle-a-list-in-haskell#14693289
-- Shuffles a list in place with Fisher-Yates-Shuffle
swapElements_ :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapElements_ arr i j = do a <- readArray arr i
                           b <- readArray arr j
                           writeArray arr i b
                           writeArray arr j a
                           return ()

shuffle :: [a] -> IO [a]
shuffle xs = do let upperBound = length xs
                arr <- (newListArray (1, upperBound) :: [a] -> IO (IOArray Int a)) xs
                mapM_ (shuffleCycle arr) [2..upperBound]
                getElems arr
  where shuffleCycle arr i = do j <- getStdRandom (randomR (1, i))
                                swapElements_ arr i j

-- | Example Exams

exam_1, exam_2 :: Exam
exam_1 = Exam {examTitle="32 Questions", examPassPercentage=0.45, examQuestions= (take 32 $ repeat q1), examAuthor = Nothing}
exam_2 = Exam {examTitle = "Intermediate Haskell", examPassPercentage = 0.8, examQuestions = [Question {questionContent = "Was ist der allgemeinste Typ von (compare . fst)?", questionAnswerList = [Answer {answerContent = "Eq b => (a, b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord b => (b, b1) -> b -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => (a,b) -> b -> Ordering", answerIsCorrect = False},Answer {answerContent = "a -> b -> Ordering ", answerIsCorrect = False}]},Question {questionContent = "Zu was ist zipWith (++) [['h','a','l','l','o']] ['w':'e':'l':'t':[]] \228quivalent?", questionAnswerList = [Answer {answerContent = "[\"hallo\", \"welt\"]", answerIsCorrect = False},Answer {answerContent = "\"hallowelt\"", answerIsCorrect = False},Answer {answerContent = "(\"hallo\" ++ \"welt\"):[]", answerIsCorrect = True},Answer {answerContent = "[\"hallowelt\"]", answerIsCorrect = True}]},Question {questionContent = "Welchen Typ hat foldl compare?", questionAnswerList = [Answer {answerContent = "Ordering -> [Ordering] -> Ordering", answerIsCorrect = True},Answer {answerContent = "Ord a => a -> [a] -> a", answerIsCorrect = False},Answer {answerContent = "[Ordering] -> Ordering -> Ordering", answerIsCorrect = False},Answer {answerContent = "Ord a => a -> [a] -> Ordering", answerIsCorrect = False}]},Question {questionContent = "Was ist der allgemeinste Typ von Just $ Just Nothing?", questionAnswerList = [Answer {answerContent = "a", answerIsCorrect = False},Answer {answerContent = "Maybe a", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe a)", answerIsCorrect = False},Answer {answerContent = "Maybe (Maybe (Maybe a))", answerIsCorrect = True}]},Question {questionContent = "Zu was ist scanl (+) 0 [1..10] \228quivalent?", questionAnswerList = [Answer {answerContent = "foldl (\\x y -> x++[sum y]) [] (inits [1..10])", answerIsCorrect = True},Answer {answerContent = "take 11 $ unfoldr (\\(x,y) -> Just (x,(x+y,y+1))) (0,1)", answerIsCorrect = True},Answer {answerContent = "[0,1,3,6,10,15,21,28,36,45,55]", answerIsCorrect = True},Answer {answerContent = "map sum $ inits [1..10]", answerIsCorrect = True}]},Question {questionContent = "Welchen allgemeinen Typ hat (*) 1.6 ?", questionAnswerList = [Answer {answerContent = "Num b => b -> b", answerIsCorrect = False},Answer {answerContent = "Integral a => a -> a", answerIsCorrect = False},Answer {answerContent = "Fractional a => a -> a", answerIsCorrect = True},Answer {answerContent = "Double -> Double", answerIsCorrect = False}]}], examAuthor = Nothing}

q1 :: Question
q1 = Question {questionContent="Wieviel ist 5+4?", questionAnswerList=q1_answers}

q1_answers :: [Answer]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]


q1_a1, q1_a2, q1_a3, q1_a4 :: Answer

q1_a1 = Answer {answerContent="9", answerIsCorrect=True }
q1_a2 = Answer {answerContent="3", answerIsCorrect=False}
q1_a3 = Answer {answerContent="7", answerIsCorrect=False}
q1_a4 = Answer {answerContent="5", answerIsCorrect=False}

