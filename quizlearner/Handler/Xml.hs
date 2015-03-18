module Handler.Xml where

import Assets (getAllExams)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget)

-- | Displays XML of selected exam in browser
getXmlR :: ExamId -> Handler Html
getXmlR examId = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams, exam) <- runDB $ do
        (publicExams, privateExams)   <- getAllExams memail
        exam                          <- get404 examId
        return (publicExams, privateExams, exam)
    let middleWidget = displayXML exam
    defaultLayout $(widgetFile "xml")

-- | Generates XML from an exam to be displayed in browser
displayXML :: Exam -> Widget
displayXML exam = let eTitle  = examTitle exam
                      percent = examPassPercentage exam
                      spaces  = [whamlet| &nbsp;&nbsp;&nbsp;&nbsp; |]
                  in
                      [whamlet|
                        <div class=xml>
                            &lt;<span class=xmlRed>!DOCTYPE</span> quiz SYSTEM <span class=xmlRed>"http://localhost:3000/static/dtd/examValidation.dtd"</span>&gt;<br>
                            &lt;<span class=xmlRed>quiz</span> title="#{eTitle}" passpercentage="#{percent}"&gt;<br>
                            $forall q <- examQuestions exam
                                ^{spaces}&lt;<span class=xmlRed>question</span> content="#{questionContent q}"&gt; <br>
                                $forall a <- questionAnswerList q
                                    ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                                    correct="#{show $ answerIsCorrect a}"&gt;#{answerContent a}&lt;<span class=xmlRed>/answer</span>&gt;<br>
                                ^{spaces}&lt;<span class=xmlRed>/question</span>&gt;<br>
                            &lt;<span class=xmlRed>/quiz</span>&gt;

                      |]