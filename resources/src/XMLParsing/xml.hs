{-# LANGUAGE Arrows, OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import Text.XML.HXT.Arrow.ReadDocument

data Exam = Exam {examTitle :: Text, examPassPercetage :: Double, examQuestions :: [Question]}
    deriving Show

data Question = Question {questionContent :: Text, questionAnswerList :: [Answer]}
    deriving Show

data Answer = Answer {answerContent :: Text, answerIsCorrect :: Bool}
    deriving Show


readPassPercentage :: String -> Double
readPassPercentage s = case reads s of
                        [(x, "")] -> x
                        _         -> -1


makeE "quiz" = Exam
makeE trans = error $ "Invalid transaction type: " ++ trans

makeQ "question" = Question
makeQ trans = error $ "Invalid transaction type: " ++ trans

makeA "answer" = Answer
makeA trans = error $ "Invalid transaction type: " ++ trans


transformE :: [Question] -> IOSLA (XIOState ()) XmlTree Exam
transformE qs = proc el -> do
  quiz <- getName -< el
  title <- getAttrValue "title" -< el
  passpercentage <- getAttrValue "passpercentage" -< el
  returnA -< (makeE quiz) (pack title) (readPassPercentage passpercentage) qs


transformQ :: [Answer] -> IOSLA (XIOState ()) XmlTree Question
transformQ as = proc el -> do
  question <- getName -< el
  content  <- getAttrValue "content" -< el
  returnA  -< (makeQ question) (pack content) as


transformA :: IOSLA (XIOState ()) XmlTree Answer
transformA = proc el -> do
  answer  <- getName -< el
  correct <- getAttrValue "correct" -< el
  content <- withDefault (Just . pack ^<< getText <<< getChildren) Nothing -< el
  returnA -< (makeA answer) (fromMaybe "" content) (elem correct ["true", "True"])


parse :: FilePath -> IO (Maybe Exam)
parse path = do
  let getAnswers   = getChildren >>> getChildren >>> getChildren >>> isElem
      getQuestions = getChildren >>> getChildren >>> isElem
      getQuiz      = getChildren >>> isElem
      valXml       = readString [withRelaxNG "validation.rng"] path
  as <- runX $ valXml >>> getAnswers >>> transformA
  qs <- mapM (\x -> runX $ valXml >>> getQuestions >>> transformQ x) $ chunksOf 4 as
  ex <- runX $ valXml >>> getQuiz >>> transformE (fst $ foldl (\(x,y) q -> (x ++ [q!!y], y + 1)) ([],0) qs)
  case ex of 
    []    -> return Nothing
    (a:_) -> return $ Just a