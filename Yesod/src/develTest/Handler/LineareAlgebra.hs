module Handler.LineareAlgebra where

import Import
--import Data.List as L ((!!), length)
--import Data.Text
--import System.Random
import TemporaryLibrary



--shuffle_answers :: [Answer] -> IO ([Answer])
--shuffle_answers list = do
--    let n = L.length list
--    seed <- getStdRandom (randomR (0, (product [1..n]) - 1))
--    return $ permutations list !! seed
--
--    shuffled_answers <- liftIO $ shuffle_answers $ answer_list snd_q

test_answer_AForm :: AForm Handler Test_Answer
test_answer_AForm = Test_Answer
  <$> areq checkBoxField "Testing...1" Nothing
  <*> areq checkBoxField "Testing...2" Nothing
  <*> areq checkBoxField "Testing...3" Nothing
  <*> areq checkBoxField "Testing...4" Nothing

answer_form :: Html -> MForm Handler (FormResult Test_Answer, Widget)
answer_form = renderTable test_answer_AForm


getLineareAlgebraR :: Handler Html
getLineareAlgebraR = do
    (widget, enctype) <- generateFormPost answer_form
    defaultLayout $
     $(widgetFile "lin_alg_form")


postLineareAlgebraR :: Handler Html
postLineareAlgebraR = do
  ((result, widget), enctype) <- runFormPost answer_form
  case result of
    FormSuccess test_answers -> defaultLayout $
          $(widgetFile "lin_alg_answers")
    _ -> defaultLayout $ [whamlet|
                          <p> Error! You dun goofed...
                          <a id=exam_title href=@{LayoutR}> Get back!>
                         |]