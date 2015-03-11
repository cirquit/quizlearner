{-# LANGUAGE Arrows, OverloadedStrings #-}

module XMLParsing where

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Text.XML.HXT.RelaxNG
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import Text.XML.HXT.Arrow.ReadDocument
import Import
import Prelude (reads, foldl, (!!))

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
  returnA -< (makeA answer) (fromMaybe "" content) (elem correct ["true", "True", "TRUE"])


parse :: String -> IO (Maybe Exam)
parse input = do
  let getAnswers   = getChildren >>> getChildren >>> getChildren >>> isElem
      getQuestions = getChildren >>> getChildren >>> isElem
      getQuiz      = getChildren >>> isElem
      valXml       = readString [withValidate yes, withHTTP []] input
  as <- runX $ valXml >>> getAnswers >>> transformA
  qs <- mapM (\x -> runX $ valXml >>> getQuestions >>> transformQ x) $ chunksOf 4 as
  ex <- runX $ valXml >>> getQuiz >>> transformE (fst $ foldl (\(x,y) q -> (x ++ [q!!y], y + 1)) ([],0) qs)
  case ex of
    []    -> return Nothing
    (a:_) -> return $ Just a

validateParsedExam :: Exam -> Bool
validateParsedExam exam = ((examTitle exam) /= (""::Text))
                       && (percentage >= 0)
                       && (percentage <= 1)
                       && (and $ map checkQuestion $ examQuestions exam)
  where
    percentage = examPassPercetage exam

    checkAnswer :: Answer -> Bool
    checkAnswer a = (answerContent a) /= (""::Text)

    checkQuestion :: Question -> Bool
    checkQuestion q = ((questionContent q) /= (""::Text)) && (and $ map checkAnswer $ questionAnswerList q)
