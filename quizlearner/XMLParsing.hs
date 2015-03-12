{-# LANGUAGE Arrows, OverloadedStrings #-}

module XMLParsing where

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.List.Split (chunksOf)
import Import
import Prelude (reads, foldl, (!!))
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Attoparsec.Text (inClass)

readPassPercentage :: String -> Double
readPassPercentage s = case reads s of
                        [(x, "")] -> x
                        _         -> -1

makeE :: [Char] -> Text -> Double -> [Question] -> Exam
makeE "quiz" = Exam
makeE trans = error $ "Invalid transaction type: " ++ trans


makeQ :: [Char] -> Text -> [Answer] -> Question
makeQ "question" = Question
makeQ trans = error $ "Invalid transaction type: " ++ trans

makeA :: [Char] -> Text -> Bool -> Answer
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


parseXml :: String -> IO (Maybe Exam)
parseXml input = do
  let getAnswers   = getChildren >>> getChildren >>> getChildren >>> isElem
      getQuestions = getChildren >>> getChildren >>> isElem
      getQuiz      = getChildren >>> isElem
      valXml       = readString [withValidate yes, withHTTP []] input
  as <- runX $ valXml >>> getAnswers >>> transformA
  qs <- mapM (\x -> runX $ valXml >>> getQuestions >>> transformQ x) $ chunksOf 4 as
  ex <- runX $ valXml >>> getQuiz >>> transformE (fst $ foldl (\(x,y) q -> (x ++ [q!!y], y + 1)) ([],0) qs)
  case ex of
    []    -> return Nothing
    (a:_) -> return $ case validateParsedExam a of
                           True -> Just a
                           _    -> Nothing

validateParsedExam :: Exam -> Bool
validateParsedExam exam = ((examTitle exam) /= (""::Text))
                       && (percentage >= 0)
                       && (percentage <= 1)
                       && (and $ map checkQuestion $ examQuestions exam)
  where
    percentage = examPassPercentage exam

    checkAnswer :: Answer -> Bool
    checkAnswer a = (answerContent a) /= (""::Text)

    checkQuestion :: Question -> Bool
    checkQuestion q = ((questionContent q) /= (""::Text)) && (and $ map checkAnswer $ questionAnswerList q)

checkDtd :: (LazySequence b s, Element s ~ Char) =>
                  b -> Either String b
checkDtd input = case parse dtdValidation "" (unpack $ toStrict $ input) of
    Left errMsg -> Left $ "No match: " ++ show errMsg
    Right _ -> Right input

spaceSkip :: Parser String
spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

dtdValidation :: Parser ()
dtdValidation = do
      _ <-  spaceSkip
      _ <-  string "<!DOCTYPE"
      _ <-  spaceSkip
      _ <-  string "quiz"
      _ <-  spaceSkip
      _ <-  string "SYSTEM"
      _ <-  spaceSkip
      _ <-  string "\"http://localhost:3000/static/dtd/examValidation.dtd\""
      _ <-  spaceSkip
      _ <-  char '>'
      return ()