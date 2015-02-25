module Handler.Exam where

import TemporaryLibrary
import Import
import qualified Data.Text as T

--create_question :: Question -> Widget
--create_question quest = do
--        m_answers <- lookupSession (questionIdentity quest)


--save_cur_answers :: Question -> WidgetT App IO()
--save_cur_answers quest = do
--            q_box1 <- runInputPost $ ireq checkBoxField (answerIdentity ((questionAnswerList quest) !! 0))
--            q_box2 <- runInputPost $ ireq checkBoxField (answerIdentity ((questionAnswerList quest) !! 1))
--            q_box3 <- runInputPost $ ireq checkBoxField (answerIdentity ((questionAnswerList quest) !! 2))
--            q_box4 <- runInputPost $ ireq checkBoxField (answerIdentity ((questionAnswerList quest) !! 3))

--            setSession (questionIdentity quest) $ bool_to_cookie [q_box1, q_box2, q_box3, q_box4]

--save_all :: WidgetT App IO[()]
--save_all = mapM save_cur_answers (examQuestions exam_1)

--validate_answers :: Question -> Widget
--validate_answers quest = do
--            m_answers <- lookupSession (questionIdentity quest)
--            case (m_answers) of
--                (Just (T.unpack -> [x1,x2,x3,x4])) -> toWidget [hamlet|
--                          <span class=evaluation> Question#{questionIdentity quest} #{questionContent quest} should be:
--                          <span class=evaluation> #{show_right_answers quest} <br>
--                          <span class=evaluation> You said that A1 := #{answerContent ((questionAnswerList quest) !! 0)} == #{cookie_to_textbool x1} <br>
--                          <span class=evaluation> You said that A2 := #{answerContent ((questionAnswerList quest) !! 1)} == #{cookie_to_textbool x2} <br>
--                          <span class=evaluation> You said that A3 := #{answerContent ((questionAnswerList quest) !! 2)} == #{cookie_to_textbool x3} <br>
--                          <span class=evaluation> You said that A4 := #{answerContent ((questionAnswerList quest) !! 3)} == #{cookie_to_textbool x4} <br>
--                          |]
--                _                                  -> toWidget[hamlet| <span class=evaluation> There are no current answers submitted|]

getExamR :: ExamId -> Handler Html
getExamR exam_id = do
              entity_exam_list <- runDB $ selectList [] [Desc ExamTitle]
              exam <-  runDB $ get404 exam_id
              (widget, enctype) <-generateFormPost $ listEditMForm $ examQuestions exam
              let middleWidget = [whamlet|  <form method=post enctype=#{enctype}>
                                                 ^{widget}
                                            <script>
                                                    var divList = document.getElementsByClassName("tab-content");
                                                        for(i=0; i<divList.length; i++){
                                                            divList[i].style.top = 40*Math.ceil(#{length $ examQuestions exam}/10) + "px";
                                                        }
                                        |]
              defaultLayout $ do $(widgetFile "exam")


postExamR :: ExamId -> Handler Html
postExamR exam_id = do
              entity_exam_list <- runDB $ selectList [] [Desc ExamTitle]
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
                                              <a href=@{LayoutR} style="margin:10px;"> Get back!
                                                                |]
                   _                  -> [whamlet| <span class=evaluation> Ups! Something went wrong!
                                                   <a href=@{LayoutR} style="margin:10px;"> Get back!
                                         |]
              defaultLayout $ do $(widgetFile "exam")


zipAnswers :: [Answer] -> [(Text, Int)]
zipAnswers [] = []
zipAnswers xs = acc 0 xs
     where  acc _ []  = []
            acc n ((Answer _ text val _ _ ):xs') =  (text, n):acc (n+1) xs'

zipQuestCookies :: [Question] -> [[Bool]] -> [(Question, Maybe [Bool])]
zipQuestCookies []      _     = []
zipQuestCookies (x:xs) []     = (x,Nothing) :zipQuestCookies xs []
zipQuestCookies (x:xs) (y:ys) = (x,(Just y)):zipQuestCookies xs ys


listEditMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Int])]), Widget)
listEditMForm xs token = do
      -- session cookie mit den fragen zippen und dann mit dem forM als argument rausholen
      -- let questsNcookies = zipQuestCookies xs []
      check_fields <- forM xs (\(Question _ content list _ ) -> mopt (checkboxesFieldList' $ zipAnswers list) (fromString $ T.unpack content) Nothing)
      let (check_results, check_views) = unzip check_fields
      let numerated_views = Import.zip ([1..]::[Int]) check_views
      let widget = [whamlet|
              ^{token}
                <ul class="tabs">
                   $forall (c,view) <- numerated_views
                       <li>
                           <input type="radio" name="tabs" id="tab#{fvId view}">
                           <label for="tab#{fvId view}">Q #{show c}
                           <div id="tab-content#{fvId view}" class="tab-content animated fadeIn">
                               <p class=boldWhite> #{fvLabel view}: </p>
                               ^{fvInput view}
                               <br>
             <input class=button type=submit value="Testing">
        |]
      return ((FormSuccess check_results), widget)



-- ################## CUSTOM FIELDS ######################
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