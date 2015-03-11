{-# LANGUAGE Arrows, OverloadedStrings #-}


--{-# LANGUAGE OverloadedStrings #-}

module XMLParsing where

--import Prelude hiding (readFile, FilePath())
--import Text.XML
--import Text.XML.Cursor
--import Data.Text as T hiding (map, length, zipWith)
--import Data.List.Split as S (chunksOf)
--import Import hiding (zipWith, putStrLn, elem, length, readFile, unpack)
--import Data.ByteString.Lazy.Internal as LB

--makeAnswerLists :: Cursor -> [[Answer]]
--makeAnswerLists cursor = zipWith (zipWith makeAnswer) aContents aCorrects
--    where
--        makeAnswer :: Text -> Text -> Answer
--        makeAnswer cont correct = Answer {
--                                          answerContent  = cont,
--                                          answerIsCorrect  = if elem correct ["true","True"] then True else False}
--        aContents   = getAnswerContents cursor
--        aCorrects   = getAnswerAttributes cursor "correct"



--makeQuestionList :: Cursor -> [Question]
--makeQuestionList cursor = zipWith makeQuestion qContents answerLists
--    where
--        makeQuestion :: Text -> [Answer] -> Question
--        makeQuestion cont answers = Question {
--                                              questionContent    = cont,
--                                              questionAnswerList = answers}
--        qContents   = getQuestionAttributes cursor "content"
--        answerLists = makeAnswerLists cursor

--makeExam :: LB.ByteString -> Maybe Exam
--makeExam bs = case parseLBS def bs of
--                   Right doc -> let cursor = fromDocument $ doc in
--                                    Just $ Exam {
--                                    examTitle          = T.concat $ attribute "title" cursor,
--                                    examPassPercentage = read (T.unpack $ T.concat $ attribute "passpercentage" cursor)::Double,
--                                    examQuestions      = makeQuestionList cursor}
--                   Left _    -> Nothing


--getQuestionAttributes :: Cursor -> Name -> [Text]
--getQuestionAttributes cursor attr = child cursor
--                                  >>= element "question"
--                                  >>= attribute attr



--getAnswerAttributes :: Cursor -> Name -> [[Text]]
--getAnswerAttributes cursor attr = S.chunksOf 4 $
--                               child cursor
--                               >>= element "question"
--                               >>= child
--                               >>= element "answer"
--                               >>= attribute attr


--getAnswerContents :: Cursor -> [[Text]]
--getAnswerContents cursor = S.chunksOf 4 $
--                           child cursor
--                           >>= element "question"
--                           >>= child
--                           >>= element "answer"
--                           >>= descendant
--                           >>= content


import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Text.XML.HXT.RelaxNG
--import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import Text.XML.HXT.Arrow.ReadDocument
import Import
import Prelude (reads, foldl, (!!))
--data Exam = Exam {examTitle :: Text, examPassPercetage :: Double, examQuestions :: [Question]}
--    deriving Show
--
--data Question = Question {questionContent :: Text, questionAnswerList :: [Answer]}
--    deriving Show
--
--data Answer = Answer {answerContent :: Text, answerIsCorrect :: Bool}
--    deriving Show


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

