module Handler.Xml where

import Import
import Widgets

getXmlR :: ExamId -> Handler Html
getXmlR examId = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    exam <- runDB $ get404 examId
    let middleWidget = displayXML exam
    defaultLayout $ do $(widgetFile "xml")

displayXML :: Exam -> Widget
displayXML exam = let eTitle  = examTitle exam
                      percent = examPassPercentage exam
                      spaces  = [whamlet| &nbsp;&nbsp;&nbsp;&nbsp; |]
                  in
                      [whamlet|
                        <div class=xml>
                            &lt;<span class=xmlRed>quiz</span> title="#{eTitle}" passpercentage="#{percent}"&gt;<br>
                            $forall q <- examQuestions exam
                                ^{spaces}&lt;<span class=xmlRed>question</span> content="#{questionContent q}"&gt; <br>
                                $forall a <- questionAnswerList q
                                    ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                                    correct="#{show $ answerIsCorrect a}"&gt;#{answerContent a}&lt;<span class=xmlRed>/answer</span>&gt;<br>
                                ^{spaces}&lt;<span class=xmlRed>/question</span>&gt;<br>
                            &lt;<span class=xmlRed>/quiz</span>&gt;

                      |]