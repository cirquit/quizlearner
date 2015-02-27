{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import Text.HTML.DOM
import Data.Text as T hiding (map, length)
import Data.List.Split as S (chunksOf)


data Exam = Exam {examTitle :: Text, examMaxScore :: Int, examTime :: Int, examPassPercentage :: Double, examQuestions :: [Question]}
    deriving Show

data Question = Question {questionIdentity :: Text, questionContent :: Text, questionAnswerList :: [Answer]}
    deriving Show

data Answer = Answer {answerIdentity :: Text, answerContent :: Text, answerCorrect :: Bool}
    deriving Show


makeAnswerLists :: Cursor -> [[Answer]]
makeAnswerLists cursor = zipWith3 (zipWith3 makeAnswer) aIdentities aContents aCorrects
    where
        makeAnswer :: Text -> Text -> Text -> Answer
        makeAnswer identity content correct = Answer {
                                              answerIdentity = identity, 
                                              answerContent  = content, 
                                              answerCorrect  = if correct=="true" then True else False}
        aIdentities = getAnswerAttributes cursor "identity"
        aContents   = getAnswerContents cursor
        aCorrects   = getAnswerAttributes cursor "correct"    



makeQuestionList :: Cursor -> [Question]
makeQuestionList cursor = zipWith3 makeQuestion qIdentities qContents answerLists
    where
        makeQuestion :: Text -> Text -> [Answer] -> Question
        makeQuestion identity content answers = Question {
                                                questionIdentity   = identity,
                                                questionContent    = content,
                                                questionAnswerList = answers}
        qIdentities = getQuestionAttributes cursor "identity"
        qContents   = getQuestionAttributes cursor "content"
        answerLists = makeAnswerLists cursor


makeExam :: Cursor -> Exam
makeExam cursor = Exam {
                  examTitle          = T.concat $ attribute "title" cursor,
                  examMaxScore       = 4 * (length $ makeAnswerLists cursor),
                  examTime           = read (T.unpack $ T.concat $ attribute "time" cursor)::Int,
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



