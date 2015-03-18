module Handler.ExampleXML where

import Assets (getAllExams)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget)

-- | Displays valid example XML
getExampleXMLR :: Handler Html
getExampleXMLR = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    let spaces  = [whamlet| &nbsp;&nbsp;&nbsp;&nbsp; |]
        middleWidget = [whamlet|
            <div style="margin:10px;">
                <span class=simpleOrange style="font-weight:bold;">_{MsgShouldLookLike}
                <p class=plainWhite style="font-weight:bold; margin-top:10px;"> _{MsgObeyRules}
                <ul class=plainWhite>
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;!DOCTYPE...&gt;</span> _{MsgDoctype}
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;quiz&gt;</span> _{MsgWithAttribute 2} "title" _{MsgAnd} "passpercentage"
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;question&gt;</span> _{MsgWithAttribute 1} "content"
                    <li> <span style="font-weight:bold; color:#FFA500;">&lt;answer&gt;</span> _{MsgWithAttribute 1} "correct"
                    <li> <span style="font-weight:bold; color:#FFA500;">"title"</span> _{MsgAnd} <span style="font-weight:bold; color:#FFA500;">"content"</span> _{MsgContainsChars}
                    <li> <span style="font-weight:bold; color:#FFA500;">"correct"</span> _{MsgContainsBool}
                    <li> <span style="font-weight:bold; color:#FFA500;">"passpercentage"</span> _{MsgContainsNumber}
                    <li> _{MsgOneQuestion}
                    <li> _{MsgFourAnswers}
                    <li> _{MsgContentTags}
                    <br>
                <span class=plainWhite style="margin-top:10px; font-weight: bold;"> _{MsgValidXML}
                <div class=xml>
                    &lt;<span class=xmlRed>!DOCTYPE</span> quiz SYSTEM <span class=xmlRed>"http://localhost:3000/static/dtd/examValidation.dtd"</span>&gt;<br>
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

                <a href=@{UploadR} style="font-size: 16px; font-weight: bold; color:white; float:right; margin:10px;"> _{MsgGotIt}
                     |]
    defaultLayout $(widgetFile "exampleXml")
