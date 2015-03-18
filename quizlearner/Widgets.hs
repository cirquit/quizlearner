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

publicExamWidget :: [Entity Exam] -> Widget
publicExamWidget exams = [whamlet|
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
    ^{colorLinkWidgets}

                            |]

privateExamWidget :: [Entity Exam] -> Widget
privateExamWidget exams = [whamlet|
    <p class=orangeTitle> [_{MsgPrivExams}]
    <ul class=examList style="padding-left: 0px;">
          $if null exams
              <p class=smallWhite style="text-align:center"> _{MsgNoPrivExams}
          $else
              $forall (Entity examId exam) <- exams
                  <li class=examList>
                      <a href=@{ExamR examId}> #{examTitle exam}
                      <a href=@{XmlR examId}> <img src="@{StaticR images_xml_svg}" title="_{MsgShowXML}" height="20px">
                      <a href=@{DeleteR examId}> <img src="@{StaticR images_trashcan_svg}" title="_{MsgDelExam}" height="25px">
    ^{colorLinkWidgets}
    ^{langWidget}
                    |]


colorLinkWidgets :: Widget
colorLinkWidgets = toWidgetBody [julius|
    function test() {
        var links = document.getElementsByTagName('a');
        for (i = 1; i < links.length; i++) {
            if (links[i].href == document.URL) {
                links[i].style.color = "#FFA500";
                return;
            }
        }
    }
    test();
                                |]

langWidget :: Widget
langWidget = [whamlet|
        <form action=@{LangR} method=post id="language-form">
            <select name=lang>
                <option value=en onclick="$('#language-form').submit();"> _{MsgEnglish}
                <option value=de onclick="$('#language-form').submit();"> _{MsgGerman}
                <option value=ru onclick="$('#language-form').submit();"> _{MsgRussian}
             |]

errorWidget  :: Text -> Widget
errorWidget text = [whamlet|
    <span class=simpleWhite> _{MsgError text}
         <a href=@{HomeR} style="margin:10px">
             <label class=simpleOrange> _{MsgGetBack}
                   |]

spacingScript :: Widget
spacingScript = toWidgetBody [julius|
        var divList = document.getElementsByClassName("tab-content");
            for(i=0; i<divList.length; i++){
                divList[i].style.top = 40*Math.ceil(divList.length/10) + "px";
            }
                |]

autoFocusById :: Text -> Widget
autoFocusById targetId = [whamlet|<script> document.getElementById("#{targetId}").focus(); |]

iconWidget :: Widget
iconWidget = do
    memail <- lookupSession "_ID"
    let accountWidget = case memail of
                            (Just email) -> [whamlet|<form action=@{AccountR "logout"} class=icons method=post>
                                                     <input type="image" src=@{StaticR images_Logout_svg} width="80px" height="80px" alt="Logout!" title=_{MsgLoogedIn email}>|]
                            _            -> [whamlet|<a href="javascript:bidClick();">
                                                            <img src=@{StaticR images_Login_svg} class=icons id="login" width="80px" height="80px">|]
    toWidget [lucius| .icons {float: right; margin: 20px;}|]
    [whamlet|
     <a href=@{QuizcreatorR}>
         <img src=@{StaticR images_quizCreator_svg} class=icons title=_{MsgQuizCreatorTitle} width="80px" height="80px">
     <a href=@{UploadR}>
         <img src=@{StaticR images_uploadXML_svg} class=icons title=_{MsgUploadXMLTitle} width="80px" height="80px">
     ^{accountWidget}
           |]