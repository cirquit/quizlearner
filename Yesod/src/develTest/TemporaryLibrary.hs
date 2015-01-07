module TemporaryLibrary where

import Import
import qualified Data.Text as T
import Database.Persist.Sqlite


-- ###################################################################################
-- Colors

col_red, col_green, col_yellow, col_blue, col_magenta, col_cyan, col_reset :: T.Text
col_red     = "\x1b[31m"
col_green   = "\x1b[32m"
col_yellow  = "\x1b[33m"
col_blue    = "\x1b[34m"
col_magenta = "\x1b[35m"
col_cyan    = "\x1b[36m"
col_reset   = "\x1b[0m"


-- ###################################################################################
-- Helper

exam_questions :: Exam -> [Question]
exam_questions (Exam _ _ _ _ list) = list

exam_title :: Exam -> T.Text
exam_title (Exam title _ _ _ _) = title

question_id :: Question -> T.Text
question_id (Question id' _ _ _) = id'

question_content :: Question -> T.Text
question_content (Question _ content _ _) = content

answer_list :: Question -> [Answer]
answer_list (Question _ _ list _) = list

answer_id :: Answer -> T.Text
answer_id (Answer id' _ _ _ _) = id'

answer_content :: Answer -> T.Text
answer_content (Answer _ content _ _ _) = content

is_correct :: Answer -> Bool
is_correct (Answer _ _ var _ _) = var

cookie_to_textbool :: Char -> T.Text
cookie_to_textbool x = if x == '1' then T.pack "True"
                                              else T.pack "False"

bool_to_cookie :: [Bool] -> T.Text
bool_to_cookie l = T.pack $ foldr (\x xs -> (bool_to_char x) : xs) "" l
  where bool_to_char :: Bool -> Char
        bool_to_char x = if x then '1' else '0'

show_right_answers :: Question -> T.Text
show_right_answers quest = T.pack $ concatMap ((++ " ") . show . is_correct) (answer_list quest)

-- ###################################################################################
-- Widgets

checkBoxWidget :: T.Text -> T.Text -> T.Text -> Widget
checkBoxWidget checked identity quest = if (T.unpack checked) == "True"
        then toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} checked>   </td> <td class=answers> <span class=answers> #{quest} </td>|]
        else toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} unchecked> </td> <td class=answers> <span class=answers> #{quest} </td>|]

titleWidget :: Widget
titleWidget = toWidget [hamlet|<a id=title href=@{LayoutR} style="text-decoration:none;">
                                   <span style="color:#FAA500;">Quiz</span>Learner<br>
                       |]

leftWidget :: [Entity Exam] -> Widget
leftWidget exams = toWidget [hamlet|<p id=exam_title> [Exams] </p>
                                       <ul id=exam_list>
                                             $if null exams
                                                 <p id=error> Couldn't find any exams in the DB!
                                             $else
                                                 $forall (Entity exam_id exam) <- exams
                                                     <li><a href=@{ExamR exam_id}> #{exam_title exam} </a>
                              |]

-- ###################################################################################
-- Static Files

staticFiles "static"

iconWidget :: Widget
iconWidget = do
             toWidget [hamlet| <img src=@{StaticR images_quizcreator_png} id="quiz_creator" title=#{q_creator_title}>
                      |]
             toWidget [lucius| #quiz_creator{float: right; margin: 30px;}
                      |]

q_creator_title :: Text
q_creator_title = T.pack "Click this if you want to create a new exam!"


-- ###################################################################################
-- DB


load_DB :: IO()
load_DB = runSqlite "develTest.sqlite3" $ do
 runMigration migrateAll

 _ <- insert $ exam_1
 _ <- insert $ exam_2
 liftIO $ putStrLn "The function load_DB was called!"

-- ###################################################################################
-- Temporary Exams

exam_list :: [Exam]
exam_list =[exam_1, exam_2]

exam_1, exam_2 :: Exam
exam_1 = Exam "Lineare Algebra" 50 120 45.0 [q1,q2,q3,q4,q5,q6,q7,q8,q9]
exam_2 = Exam "FFP" 40 180 30.0 [q1,q2,q3]

-- Temporary Questions
q1,q2,q3,q4,q5,q6,q7,q8,q9 :: Question
q1 = Question (T.pack "1") (T.pack "Wieviel ist 2+3?") q1_answers 4
q2 = Question (T.pack "2") (T.pack "Wieviel ist 3+4?") q2_answers 4
q3 = Question (T.pack "3") (T.pack "Wieviel ist 4+5?") q3_answers 4
q4 = Question (T.pack "4") (T.pack "Wieviel ist 2+3?") q4_answers 4
q5 = Question (T.pack "5") (T.pack "Wieviel ist 3+4?") q5_answers 4
q6 = Question (T.pack "6") (T.pack "Wieviel ist 4+5?") q6_answers 4
q7 = Question (T.pack "7") (T.pack "Wieviel ist 2+3?") q7_answers 4
q8 = Question (T.pack "8") (T.pack "Wieviel ist 3+4?") q8_answers 4
q9 = Question (T.pack "9") (T.pack "Wieviel ist 4+5?") q9_answers 4

-- Temporary Answers
q1_answers, q2_answers, q3_answers, q4_answers, q5_answers, q6_answers, q7_answers, q8_answers, q9_answers :: [Answer]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]
q2_answers = [q2_a1,q2_a2,q2_a3,q2_a4]
q3_answers = [q3_a1,q3_a2,q3_a3,q3_a4]
q4_answers = [q4_a1,q4_a2,q4_a3,q4_a4]
q5_answers = [q5_a1,q5_a2,q5_a3,q5_a4]
q6_answers = [q6_a1,q6_a2,q6_a3,q6_a4]
q7_answers = [q7_a1,q7_a2,q7_a3,q7_a4]
q8_answers = [q8_a1,q8_a2,q8_a3,q8_a4]
q9_answers = [q9_a1,q9_a2,q9_a3,q9_a4]


q1_a1, q1_a2, q1_a3, q1_a4, q2_a1, q2_a2, q2_a3, q2_a4, q3_a1, q3_a2, q3_a3, q3_a4 :: Answer
q4_a1, q4_a2, q4_a3, q4_a4, q5_a1, q5_a2, q5_a3, q5_a4, q6_a1, q6_a2, q6_a3, q6_a4 :: Answer
q7_a1, q7_a2, q7_a3, q7_a4, q8_a1, q8_a2, q8_a3, q8_a4, q9_a1, q9_a2, q9_a3, q9_a4 :: Answer

q1_a1 = Answer (T.pack "q1_a1") (T.pack "1") False False (T.pack "This is hint")
q1_a2 = Answer (T.pack "q1_a2") (T.pack "2") False False (T.pack "This is hint")
q1_a3 = Answer (T.pack "q1_a3") (T.pack "3") False False (T.pack "This is hint")
q1_a4 = Answer (T.pack "q1_a4") (T.pack "5") True  False (T.pack "This is hint")
q2_a1 = Answer (T.pack "q2_a1") (T.pack "5") False False (T.pack "This is hint")
q2_a2 = Answer (T.pack "q2_a2") (T.pack "9") False False (T.pack "This is hint")
q2_a3 = Answer (T.pack "q2_a3") (T.pack "3") False False (T.pack "This is hint")
q2_a4 = Answer (T.pack "q2_a4") (T.pack "1") True  False (T.pack "This is hint")
q3_a1 = Answer (T.pack "q3_a1") (T.pack "1") False False (T.pack "This is hint")
q3_a2 = Answer (T.pack "q3_a2") (T.pack "9") False False (T.pack "This is hint")
q3_a3 = Answer (T.pack "q3_a3") (T.pack "7") False False (T.pack "This is hint")
q3_a4 = Answer (T.pack "q3_a4") (T.pack "2") True  False (T.pack "This is hint")
q4_a1 = Answer (T.pack "q4_a1") (T.pack "1") False False (T.pack "This is hint")
q4_a2 = Answer (T.pack "q4_a2") (T.pack "2") False False (T.pack "This is hint")
q4_a3 = Answer (T.pack "q4_a3") (T.pack "4") False False (T.pack "This is hint")
q4_a4 = Answer (T.pack "q4_a4") (T.pack "5") True  False (T.pack "This is hint")
q5_a1 = Answer (T.pack "q5_a1") (T.pack "5") False False (T.pack "This is hint")
q5_a2 = Answer (T.pack "q5_a2") (T.pack "2") False False (T.pack "This is hint")
q5_a3 = Answer (T.pack "q5_a3") (T.pack "3") False False (T.pack "This is hint")
q5_a4 = Answer (T.pack "q5_a4") (T.pack "1") True  False (T.pack "This is hint")
q6_a1 = Answer (T.pack "q6_a1") (T.pack "1") False False (T.pack "This is hint")
q6_a2 = Answer (T.pack "q6_a2") (T.pack "9") False False (T.pack "This is hint")
q6_a3 = Answer (T.pack "q6_a3") (T.pack "7") False False (T.pack "This is hint")
q6_a4 = Answer (T.pack "q6_a4") (T.pack "2") True  False (T.pack "This is hint")
q7_a1 = Answer (T.pack "q7_a1") (T.pack "1") False False (T.pack "This is hint")
q7_a2 = Answer (T.pack "q7_a2") (T.pack "2") False False (T.pack "This is hint")
q7_a3 = Answer (T.pack "q7_a3") (T.pack "4") False False (T.pack "This is hint")
q7_a4 = Answer (T.pack "q7_a4") (T.pack "5") True  False (T.pack "This is hint")
q8_a1 = Answer (T.pack "q8_a1") (T.pack "5") False False (T.pack "This is hint")
q8_a2 = Answer (T.pack "q8_a2") (T.pack "2") False False (T.pack "This is hint")
q8_a3 = Answer (T.pack "q8_a3") (T.pack "3") False False (T.pack "This is hint")
q8_a4 = Answer (T.pack "q8_a4") (T.pack "1") True  False (T.pack "This is hint")
q9_a1 = Answer (T.pack "q9_a1") (T.pack "1") False False (T.pack "This is hint")
q9_a2 = Answer (T.pack "q9_a2") (T.pack "9") False False (T.pack "This is hint")
q9_a3 = Answer (T.pack "q9_a3") (T.pack "7") False False (T.pack "This is hint")
q9_a4 = Answer (T.pack "q9_a4") (T.pack "2") True  False (T.pack "This is hint")


-- ######## DEPRICATED ###############
-- shuffle_answers :: [Answer] -> IO ([Answer])
-- shuffle_answers list = do
--     let n = L.length list
--     seed <- getStdRandom (randomR (0, (product [1..n]) - 1))
--     return $ permutations list !! seed
--
--     shuffled_answers <- liftIO $ shuffle_answers $ answer_list snd_q