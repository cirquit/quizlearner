module Handler.Exam where

import Assets (zipAnswers, titleWidget, iconWidget, leftWidget)
import Import

getExamR :: ExamId -> Handler Html
getExamR exam_id = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    exam <-  runDB $ get404 exam_id
    (widget, enctype) <-generateFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = [whamlet|
                           <form method=post enctype=#{enctype}>
                               ^{widget}
                       |]
    defaultLayout $ do $(widgetFile "exam")


postExamR :: ExamId -> Handler Html
postExamR exam_id = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    exam  <- runDB $ get404 exam_id
    ((res,_), _) <- runFormPost $ listEditMForm $ examQuestions exam
    let middleWidget = case res of
         (FormSuccess list) -> let newList = Import.zip ([1..]::[Int]) list in
                               [whamlet|
                                        $forall (c,(FormSuccess may)) <- newList
                                            $maybe just <- may
                                                <p class=simpleWhite>Question Nr.#{show c}: #{show just}
                                            $nothing
                                                <p class=simpleWhite>Question Nr.#{show c}: You didn't check any of the answers.
                                    <a href=@{HomeR} style="margin:10px;"> Get back!
                               |]
         _                  -> [whamlet|
                                         <span class=evaluation> Ups! Something went wrong!
                                         <a href=@{HomeR} style="margin:10px;"> Get back!
                               |]
    defaultLayout $ do $(widgetFile "exam")

listEditMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Int])]), Widget)
listEditMForm xs token = do
    let checkBoxes = checkboxesFieldList' . zipAnswers
    let questionStr = fromString . unpack
    checkFields <- forM xs (\(Question _ content list _ ) -> mopt (checkBoxes list) (questionStr content) Nothing)
    let (checkResults, checkViews) = unzip checkFields
    let numeratedViews = Import.zip ([1..]::[Int]) checkViews
    let widget = [whamlet|
                         ^{token}
                             <ul class="tabs">
                                 $forall (c,view) <- numeratedViews
                                     <li>
                                         <input type="radio" name="tabs" id="tab#{fvId view}">
                                         <label for="tab#{fvId view}">Q #{show c}
                                         <div id="tab-content#{fvId view}" class="tab-content animated fadeIn">
                                             <p class=boldWhite> #{fvLabel view}: </p>
                                             ^{fvInput view}
                                             <br>
                     <input class=button type=submit value="Testing">
                 |]
    return ((FormSuccess checkResults), widget)



checkboxesFieldList' :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg) =>
                              [(msg, a)] -> Field (HandlerT site IO) [a]
checkboxesFieldList' = checkboxesField' . optionsPairs


checkboxesField' :: (Eq a, RenderMessage site FormMessage) =>
                    HandlerT site IO (OptionList a) -> Field (HandlerT site IO) [a]
checkboxesField' ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val _ -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            [whamlet|
                <span ##{theId}>
                    $forall opt <- opts
                        <label>
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                            <span class=simpleWhite> #{optionDisplay opt}
            |]}

scriptWidget :: Exam -> Widget
scriptWidget exam = toWidget [hamlet|
                                 <script>
                                     var divList = document.getElementsByClassName("tab-content");
                                         for(i=0; i<divList.length; i++){
                                             divList[i].style.top = 35*Math.ceil(#{length $ examQuestions exam}/10) + "px";
                                         }
                             |]