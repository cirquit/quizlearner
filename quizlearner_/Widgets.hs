module Widgets where

import Import

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
    <p class=orangeTitle> [_{MsgExams}]
    <ul class=examList style="padding-left: 0px;">
          $if null exams
              <p class=sadred>_{MsgNoExamsInDB}
          $else
              $forall (Entity examId exam) <- exams
                  <li class=examList>
                      <a href=@{ExamR examId}> #{examTitle exam}
                      <a href=@{XmlR examId}> <img src="@{StaticR images_xml_svg}" title="_{MsgShowXML}" height="20px">
                      <a href=@{DeleteR examId}> <img src="@{StaticR images_trashcan_svg}" title="_{MsgDelExam}" height="25px">
    ^{langWidget}
                            |]

langWidget :: Widget
langWidget = [whamlet|
        <form action=@{LangR} method=post id="language-form">
            <select name=lang>
                <option value=en onclick="document.getElementById('language-form').submit();"> _{MsgEnglish}
                <option value=de onclick="document.getElementById('language-form').submit();"> _{MsgGerman}
                <option value=ru onclick="document.getElementById('language-form').submit();"> _{MsgRussian}
             |]

errorWidget  :: Text -> Widget
errorWidget text = [whamlet|
    <span class=simpleWhite> _{MsgError text}
         <a href=@{HomeR} style="margin:10px">
             <label class=simpleOrange> _{MsgGetBack}
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
    toWidget [lucius| #quiz_creator, #upload_xml {float: right; margin: 30px;}|]
    [whamlet|
     <a href=@{QuizcreatorR}>
         <img src=@{StaticR images_quizCreator_svg} id="quiz_creator" title=_{MsgQuizCreatorTitle} width="80px" height="80px">
     <a href=@{UploadR}>
         <img src=@{StaticR images_uploadXML_svg} id="upload_xml" title=_{MsgUploadXMLTitle} width="80px" height="80px">
           |]



