module Handler.Layout where

import Import
import TemporaryLibrary

titleWidget :: Widget
titleWidget = toWidget [hamlet| <div id=top class=front>
                                    <a id=title href=@{LayoutR}> QuizLearner
                       |]

leftWidget :: Widget
leftWidget = toWidget [hamlet| <ul id=exam_list>
                                    <li> <a id=exam_title href=@{LayoutR}> Exams </a>
                                    $forall single_exam <- exams
                                        <li><a class=button href=@{ExamOneR}> #{exam_title single_exam} </a>
 |]

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  $(widgetFile "layout")