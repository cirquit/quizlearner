{-# LANGUAGE Arrows #-}

module XMLParsing where

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.List.Split (chunksOf)
import Import
import Prelude (reads, foldl, (!!))
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Attoparsec.Text (inClass)

-- | Custom "read" for pass percentage
readPassPercentage :: String -> Double
readPassPercentage s = case reads s of
                          [(x, "")] -> x
                          (_)       -> -1

-- | Functions for construction of exams, questions and answers
makeE :: [Char] -> Text -> Double -> [Question] -> Maybe Text -> Exam
makeE "quiz" = Exam
makeE trans = error $ "Invalid transaction type: " ++ trans

makeQ :: [Char] -> Text -> [Answer] -> Question
makeQ "question" = Question
makeQ trans = error $ "Invalid transaction type: " ++ trans

makeA :: [Char] -> Text -> Bool -> Answer
makeA "answer" = Answer
makeA trans = error $ "Invalid transaction type: " ++ trans

-- | Extracts exam attributes from "quiz" tag
getE :: [Question] -> IOSLA (XIOState ()) XmlTree (Maybe Text -> Exam)
getE qs = proc tag -> do
    quiz           <- getName -< tag
    title          <- getAttrValue "title" -< tag
    passpercentage <- getAttrValue "passpercentage" -< tag
    returnA        -< (makeE quiz) (pack title) (readPassPercentage passpercentage) qs

-- | Extracts question attributes from "question" tag
getQ :: [Answer] -> IOSLA (XIOState ()) XmlTree Question
getQ as = proc tag -> do
    question <- getName -< tag
    content  <- getAttrValue "content" -< tag
    returnA  -< (makeQ question) (pack content) as

-- | Extracts answer attributes from "answer" tag
getA :: IOSLA (XIOState ()) XmlTree Answer
getA = proc tag -> do
    answer  <- getName -< tag
    correct <- getAttrValue "correct" -< tag
    content <- withDefault (Just . pack ^<< getText <<< getChildren) Nothing -< tag
    returnA -< (makeA answer) (fromMaybe "" content) (elem correct ["true", "True", "TRUE"])

-- | Parses XML string into an exam
parseXml :: String -> IO (Maybe (Maybe Text -> Exam))
parseXml input = do
    let getAnswers   = getChildren >>> getChildren >>> getChildren >>> isElem
        getQuestions = getChildren >>> getChildren >>> isElem
        getQuiz      = getChildren >>> isElem
        valXml       = readString [withValidate yes, withHTTP []] input
    as <- runX $ valXml >>> getAnswers >>> getA
    qs <- mapM (\x -> runX $ valXml >>> getQuestions >>> getQ x) $ chunksOf 4 as
    ex <- runX $ valXml >>> getQuiz >>> getE (fst $ foldl (\(x,y) q -> (x ++ [q!!y], y + 1)) ([],0) qs)
    case ex of
        []    -> return Nothing
        (a:_) -> return $ case validateParsedExam (a Nothing) of
                               True -> Just a
                               (_)  -> Nothing

-- | Checks if generated exam is valid
validateParsedExam :: Exam -> Bool
validateParsedExam exam = ((examTitle exam) /= (""::Text))
                       && (percentage >= 0)
                       && (percentage <= 100)
                       && (and $ map checkQuestion $ examQuestions exam)
  where
    percentage = examPassPercentage exam

    checkAnswer :: Answer -> Bool
    checkAnswer a = (answerContent a) /= (""::Text)

    checkQuestion :: Question -> Bool
    checkQuestion q = ((questionContent q) /= (""::Text)) && (and $ map checkAnswer $ questionAnswerList q)

-- | Checks if DTD validation is embeded in submitted file
checkDtd :: (LazySequence b s, Element s ~ Char) => b -> Either String b
checkDtd input = case parse dtdValidation "" (unpack $ toStrict $ input) of
    Left errMsg -> Left $ "No match: " ++ show errMsg
    Right _ -> Right input

spaceSkip :: Parser String
spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

dtdValidation :: Parser ()
dtdValidation = do
      (_) <-  spaceSkip
      (_) <-  string "<!DOCTYPE"
      (_) <-  spaceSkip
      (_) <-  string "quiz"
      (_) <-  spaceSkip
      (_) <-  string "SYSTEM"
      (_) <-  spaceSkip
      (_) <-  string "\"http://localhost:3000/static/dtd/examValidation.dtd\""
      (_) <-  spaceSkip
      (_) <-  char '>'
      return ()