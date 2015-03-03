module Handler.Xml where

import Assets
import Import

{-
data Exam = Exam {examTitle :: Text, examMaxScore :: Int, examTime :: Int, examPassPercentage :: Double, examQuestions :: [Question]}
    deriving Show

data Question = Question {questionContent :: Text, questionAnswerList :: [Answer]}
    deriving Show

data Answer = Answer {answerContent :: Text, answerCorrect :: Bool}
    deriving Show
-}

getXmlR :: ExamId -> Handler Html
getXmlR examId = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    exam <- runDB $ get404 examId
    let e = "<exam>" :: String
    let middleWidget = displayXML exam
    defaultLayout $ do $(widgetFile "xml")


displayXML :: Exam -> Widget
displayXML exam = let eTitle  = examTitle exam
                      qcount  = show $ length $ examQuestions exam
                      time    = examMaxTime exam
                      percent = examPassPercentage exam
                      spaces  = [whamlet| &nbsp;&nbsp;&nbsp;&nbsp; |]
                  in
                      [whamlet|
                        <div class=xml>
                            &lt;<span class=xmlRed>quiz</span> title="#{eTitle}" qcount="#{qcount}" time="#{time}" passpercentage="#{percent}"&gt;<br>
                            $forall q <- examQuestions exam
                                ^{spaces}&lt;<span class=xmlRed>question</span> content=#{questionContent q}&gt; <br>
                                $forall a <- questionAnswerList q
                                    ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span> 
                                    correct=#{show $ answerIsCorrect a}&gt;#{answerContent a}&lt;<span class=xmlRed>/answer</span>&gt;<br>
                                ^{spaces}&lt;<span class=xmlRed>/question</span>&gt;<br>
                            &lt;<span class=xmlRed>/quiz</span>&gt;

                      |]