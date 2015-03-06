module Handler.ExampleXML where

import Widgets
import Import

getExampleXMLR :: Handler Html
getExampleXMLR = do
    entityExamList <- runDB $ selectList [] [Asc ExamTitle]
    let spaces  = [whamlet| &nbsp;&nbsp;&nbsp;&nbsp; |]
        middleWidget = [whamlet|
            <div style="margin:20px;">
                <span class=simpleOrange style="font-weight:bold;"> What should my XML look like?
                <p class=plainWhite style="font-weight:bold; margin-top:10px;"> Your file should obey the following rules:
                <ul class=plainWhite>
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;quiz&gt;</span> with the attributes "title" and "passpercentage"
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;question&gt;</span> with the attribute "content"
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;answer&gt;</span> with the attribute "correct"
                    <li> <span style="font-weight:bold; color:#FFA500;">"title"</span> and <span style="font-weight:bold; color:#FFA500;">"content"</span> contain alphanumerical characters
                    <li> <span style="font-weight:bold; color:#FFA500;">"correct"</span> contains either "true" or "false"
                    <li> <span style="font-weight:bold; color:#FFA500;">"passpercentage"</span> contains a number between 0 and 1
                    <li> your exam has to contain at least one question
                    <li> every question has to have exactly four answers 
                    <li> the answer content has to be between the answer tags
                    <br>
                <span class=plainWhite style="margin-top:10px; font-weight: bold;"> A valid XML file might look like this:
                <div class=xml>
                    &lt;<span class=xmlRed>quiz</span> title="My Exam" passpercentage="0.4"&gt;<br>
                        ^{spaces}&lt;<span class=xmlRed>question</span> content="Which one of these is not an animal?"&gt; <br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="false"&gt;Dog&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="false"&gt;Cat&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="true"&gt;Car&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="false"&gt;Giraffe&lt;<span class=xmlRed>/answer</span>&gt;<br>
                        ^{spaces}&lt;<span class=xmlRed>/question</span>&gt;<br>
                        ^{spaces}&lt;<span class=xmlRed>question</span> content="What's 2 + 4?"&gt; <br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="true"&gt;5 + 1&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="false"&gt;4&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="false"&gt;5&lt;<span class=xmlRed>/answer</span>&gt;<br>
                            ^{spaces}^{spaces}&lt;<span class=xmlRed>answer</span>
                            correct="true"&gt;6&lt;<span class=xmlRed>/answer</span>&gt;<br>
                        ^{spaces}&lt;<span class=xmlRed>/question</span>&gt;<br>
                    &lt;<span class=xmlRed>/quiz</span>&gt;
                     |]
    defaultLayout $ do $(widgetFile "exampleXml")
