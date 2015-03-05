{-# LANGUAGE OverloadedStrings #-}

module XMLParsing where

import Prelude hiding (readFile, FilePath())
import Text.XML
import Text.XML.Cursor
import Data.Text as T hiding (map, length, zipWith)
import Data.List.Split as S (chunksOf)
import Import hiding (zipWith, putStrLn, elem, length, readFile, unpack)

makeAnswerLists :: Cursor -> [[Answer]]
makeAnswerLists cursor = zipWith (zipWith makeAnswer) aContents aCorrects
    where
        makeAnswer :: Text -> Text -> Answer
        makeAnswer content correct = Answer {
                                              answerContent  = content, 
                                              answerIsCorrect  = if elem correct ["true","True"] then True else False}
        aContents   = getAnswerContents cursor
        aCorrects   = getAnswerAttributes cursor "correct"    



makeQuestionList :: Cursor -> [Question]
makeQuestionList cursor = zipWith makeQuestion qContents answerLists
    where
        makeQuestion :: Text -> [Answer] -> Question
        makeQuestion content answers = Question {
                                                questionContent    = content,
                                                questionAnswerList = answers}
        qContents   = getQuestionAttributes cursor "content"
        answerLists = makeAnswerLists cursor


makeExam bs = let cursor = fromDocument $ parseLBS_ def bs in
                  Exam {
                  examTitle          = T.concat $ attribute "title" cursor,
                  examMaxScore       = 4 * (length $ makeAnswerLists cursor),
                  examMaxTime        = read (T.unpack $ T.concat $ attribute "time" cursor)::Int,
                  examPassPercentage = read (T.unpack $ T.concat $ attribute "passpercentage" cursor)::Double,
                  examQuestions      = makeQuestionList cursor}


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



--parse :: FilePath -> IO ()
--parse file = do
--    doc <- readFile def file
--    let cursor = fromDocument doc
--    putStrLn $ show $ makeExam cursor


