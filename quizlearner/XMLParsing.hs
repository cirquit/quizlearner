{-# LANGUAGE OverloadedStrings #-}

module XMLParsing where

import Prelude hiding (readFile, FilePath())
import Text.XML
import Text.XML.Cursor
import Data.Text as T hiding (map, length, zipWith)
import Data.List.Split as S (chunksOf)
import Import hiding (zipWith, putStrLn, elem, length, readFile, unpack)
import Data.ByteString.Lazy.Internal as LB

makeAnswerLists :: Cursor -> [[Answer]]
makeAnswerLists cursor = zipWith (zipWith makeAnswer) aContents aCorrects
    where
        makeAnswer :: Text -> Text -> Answer
        makeAnswer cont correct = Answer {
                                          answerContent  = cont,
                                          answerIsCorrect  = if elem correct ["true","True"] then True else False}
        aContents   = getAnswerContents cursor
        aCorrects   = getAnswerAttributes cursor "correct"



makeQuestionList :: Cursor -> [Question]
makeQuestionList cursor = zipWith makeQuestion qContents answerLists
    where
        makeQuestion :: Text -> [Answer] -> Question
        makeQuestion cont answers = Question {
                                              questionContent    = cont,
                                              questionAnswerList = answers}
        qContents   = getQuestionAttributes cursor "content"
        answerLists = makeAnswerLists cursor

makeExam :: LB.ByteString -> Maybe Exam
makeExam bs = case parseLBS def bs of
                   Right doc -> let cursor = fromDocument $ doc in
                                    Just $ Exam {
                                    examTitle          = T.concat $ attribute "title" cursor,
                                    examPassPercentage = read (T.unpack $ T.concat $ attribute "passpercentage" cursor)::Double,
                                    examQuestions      = makeQuestionList cursor}
                   Left _    -> Nothing


getQuestionAttributes :: Cursor -> Name -> [Text]
getQuestionAttributes cursor attr = child cursor
                                  >>= element "question"
                                  >>= attribute attr



getAnswerAttributes :: Cursor -> Name -> [[Text]]
getAnswerAttributes cursor attr = S.chunksOf 4 $
                               child cursor
                               >>= element "question"
                               >>= child
                               >>= element "answer"
                               >>= attribute attr


getAnswerContents :: Cursor -> [[Text]]
getAnswerContents cursor = S.chunksOf 4 $
                           child cursor
                           >>= element "question"
                           >>= child
                           >>= element "answer"
                           >>= descendant
                           >>= content

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