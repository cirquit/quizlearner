module Widgets where

import Import --hiding (images_quizCreator_svg, images_uploadXML_svg, images_xml_svg, images_trashcan_svg)

--staticFiles "static"

showExamWidget :: Exam -> Widget
showExamWidget  (Exam title maxScore maxTime passPercentage qList) = [whamlet|
    <p class=simpleWhite> Examtitle: #{title}
    <p class=simpleWhite> MaxScore: #{show maxScore}
    <p class=simpleWhite> MaxTime: #{show maxTime}
    <p class=simpleWhite> PassPercentage: #{show passPercentage}
    $forall (Question content aList) <- qList
        <p class=simpleWhite> Question: #{content}
            $forall (Answer aContent isCorrect) <- aList
                 <p class=simpleWhite> Answer: #{aContent} #{show isCorrect}
                                                                  |]

postWidget :: Enctype -> Widget -> Widget
postWidget enctype widget =  [whamlet|
    <form method=post enctype=#{enctype}>
        ^{widget}
                             |]

titleWidget :: Widget
titleWidget = [whamlet|
    <a class=mainTitle href=@{HomeR}>
        <span style="color:#FAA500;">Quiz</span>Learner<br>
              |]

leftWidget :: [Entity Exam] -> Widget
leftWidget exams = [whamlet|
    <p class=orangeTitle> [Exams]
    <ul class=examList style="padding-left: 0px;">
          $if null exams
              <p class=sadred> Couldn't find any exams in the DB! Please refresh!
          $else
              $forall (Entity examId exam) <- exams
                  <li class=examList>
                      <a href=@{ExamR examId}> #{examTitle exam}
                      <a href=@{XmlR examId}> <img src=@{StaticR images_xml_svg} title="Show XML" height="20px">
                      <a href=@{DeleteR examId}> <img src=@{StaticR images_trashcan_svg} title="Delete exam" height="25px">
                            |]

errorWidget  :: Text -> Widget
errorWidget text = [whamlet|
    <span class=simpleWhite> Something went wrong...in #{text}
         <a href=@{HomeR} style="margin:10px">
             <label class=simpleOrange> Get back!
                   |]

spacingScript :: Widget
spacingScript = [whamlet|
    <script>
        var divList = document.getElementsByClassName("tab-content");
            for(i=0; i<divList.length; i++){
                divList[i].style.top = 40*Math.ceil(divList.length/10) + "px";
            }
                |]

iconWidget :: Widget
iconWidget = do
    let q_creator_title = pack "Click this to create a new exam" :: Text
    let upload_xml_title = pack "Click this to upload an XML file" :: Text
    toWidget [lucius| #quiz_creator, #upload_xml {float: right; margin: 30px;}|]
    [whamlet|
     <a href=@{QuizcreatorR}>
         <img src=@{StaticR images_quizCreator_svg} id="quiz_creator" title=#{q_creator_title} width="80px" height="80px">
     <a href=@{UploadR}>
         <img src=@{StaticR images_uploadXML_svg} id="upload_xml" title=#{upload_xml_title} width="80px" height="80px">
           |]



