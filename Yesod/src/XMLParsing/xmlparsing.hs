{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
-- import Text.HTML.DOM
import Data.Text as T hiding (map, length, zipWith)
import Data.List.Split as S (chunksOf)
-- import Text.XML.HaXml


data Exam = Exam {examTitle :: Text, examMaxScore :: Int, examMaxTime :: Int, examPassPercentage :: Double, examQuestions :: [Question]}
    deriving Show

data Question = Question {questionContent :: Text, questionAnswerList :: [Answer]}
    deriving Show

data Answer = Answer {answerContent :: Text, answerIsCorrect :: Bool}
    deriving Show





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


makeExam :: Cursor -> Exam
makeExam cursor = Exam {
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



main :: IO ()
main = do
    doc <- readFile def "test2.xml"
    let cursor = fromDocument doc
    putStrLn $ show $ makeExam cursor
    


