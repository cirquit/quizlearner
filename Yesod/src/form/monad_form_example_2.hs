{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Main where

import Yesod
import Data.Text as T hiding (zip)
import Control.Applicative
import Control.Monad (forM)
import Yesod.Form
import Data.String


main :: IO ()
main = warp 3000 CarApp


data CarApp = CarApp

instance Yesod CarApp

instance RenderMessage CarApp FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "CarApp" [parseRoutes|
/          HomeR GET POST
|]

t1, t2, t3, t4 :: (String,Bool)
t1 = ("t1 - should be false", False)
t2 = ("t2 - should be true ", True)
t3 = ("t3 - should be true ", True)
t4 = ("t4 - should be false", False)

list = [t1,t2,t3,t4] :: [(String, Bool)]


data Question = Question {
                qidentity :: Text
              , qcontent :: Text
              , answerList :: [Answer]
              , maxScore :: Int
               } deriving Show

data Answer = Answer {
                 aidentity :: Text
               , acontent :: Text
               , isCorrect :: Bool
               , hint :: Text
               } deriving Show

q1,q2 :: Question
q1 = Question {qidentity = "Nr.1", qcontent="Wieviel ist 2+3?", answerList= q1_answers, maxScore=4}
q2 = Question {qidentity = "Nr.2", qcontent="Wieviel ist 3+4?", answerList= q2_answers, maxScore=4}

q1_answers, q2_answers :: [Answer]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]
q2_answers = [q2_a1,q2_a2,q2_a3,q2_a4]



q1_a1, q1_a2, q1_a3,q1_a4,q2_a1, q2_a2, q2_a3, q2_a4 :: Answer
q1_a1 = Answer {aidentity="id_1", acontent="1", isCorrect=False, hint="This is hint!"}
q1_a2 = Answer {aidentity="id_2", acontent="2", isCorrect=False, hint="This is hint!"}
q1_a3 = Answer {aidentity="id_3", acontent="3", isCorrect=False, hint="This is hint!"}
q1_a4 = Answer {aidentity="id_4", acontent="5", isCorrect=True , hint="This is hint!"}
q2_a1 = Answer {aidentity="id_5", acontent="5", isCorrect=False, hint="This is hint!"}
q2_a2 = Answer {aidentity="id_6", acontent="9", isCorrect=False, hint="This is hint!"}
q2_a3 = Answer {aidentity="id_7", acontent="7", isCorrect=True , hint="This is hint!"}
q2_a4 = Answer {aidentity="id_8", acontent="1", isCorrect=False, hint="This is hint!"}


--q1_a1, q1_a2, q1_a3,q1_a4,q2_a1, q2_a2, q2_a3, q2_a4 :: (Text, Bool)

--q1_a1 = ("1",False)
--q1_a2 = ("2",False)
--q1_a3 = ("3",False)
--q1_a4 = ("5",True )

--q2_a1 = ("5",False)
--q2_a2 = ("9",True )
--q2_a3 = ("3",False)
--q2_a4 = ("1",True )


zip_a :: [Answer] -> [(Text, Bool)]
zip_a [] = []
zip_a ((Answer _ text val _ ):xs) = (text, val):zip_a xs

listEditMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Bool])]), Widget)
listEditMForm xs token = do
      check_fields <- forM xs (\(Question _ content list _ ) -> mopt (checkboxesFieldList' $ zip_a list) (fromString $ T.unpack content) Nothing)
      let (check_results, check_views) = unzip check_fields
      let widget = [whamlet|
           ^{token}
                 <h1>Multi Field Form
                $forall view <- check_views
                 <br>
                 #{fvLabel view}:
                 ^{fvInput view}
                 <br>
              <input type=submit value="Testing mForms">
          <br>
        |]
      return ((FormSuccess check_results), widget)



getHomeR :: Handler Html
getHomeR = do
       (widget, enctype) <-generateFormPost $ listEditMForm [q1, q2]
       defaultLayout [whamlet|
              <form method=post enctype=#{enctype}>
                  ^{widget}
                     |]



--evalAnswersSum :: [Just [Bool]] -- Answers
--              ->  [Int]         -- count of answers for every question
--              ->  Int           -- exam maxscore
--              ->  Text          -- eval text
--evalAnswersSum [] _ _ = []
--evalAnswersSum l1 l2 score = count l1 l2 (0,score) 1
--               count :: [Just [Bool]] -> [Int] -> (Int,Int) -> Int -> Text
--               count (x:xs) (y:ys) (curScore,maxScore) c = case x of
--                            Nothing -> "Question Nr." ++ show c ++ " Nothing was checked out of " ++ show y ++ " possible checks. Current Score: " ++ show curScore ++ " of max. Score: " ++ show maxScore            ++ count xs ys
--                            Just l  -> "Question Nr." ++ show c ++ " This was checked" ++ show l ++ " out of " show y ++ " possible checks. Current Score: " ++ show curScore ++ " of max. Score: " ++ show maxScore  ++ count xs ys


postHomeR :: Handler Html
postHomeR = do
       ((res,widget), enctype) <- runFormPost $ listEditMForm [q1, q2]
       case res of
          FormSuccess(list) ->  defaultLayout [whamlet|
                                        $forall (c,(FormSuccess may)) <- newList
                                            $maybe just <- may
                                             <p> Question Nr.#{show c}: #{show may}
                                            $nothing
                                             <p> Question Nr.#{show c}: You didn't check any of the answers.
                                             <br>
                                        <br>
                                       <a href=@{HomeR}> Get back!
                                              |] where newList = zip [1..] list
-- eval todo:
-- 2 modi -> Nr.1 = negative points trough the exam. Nr2 = no negative points in any question
-- check how many true's, false's, or Nothing



---- | Creates an 'OptionList' from a list of (display-value, internal value) pairs.
--optionsPairs :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
--             => [(msg, a)] -> m (OptionList a)
--optionsPairs opts = do
--  mr <- getMessageRender
--  let mkOption external (display, internal) =
--          Option { optionDisplay       = mr display
--                 , optionInternalValue = internal
--                 , optionExternalValue = pack $ show external
--                 }
--  return $ mkOptionList (zipWith mkOption [1 :: Int ..] opts)




checkboxesFieldList' :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg) => [(msg, a)]
                     -> Field (HandlerT site IO) [a]
checkboxesFieldList' = checkboxesField' . optionsPairs

checkboxesField' :: (Eq a, RenderMessage site FormMessage)
                 => HandlerT site IO (OptionList a)
                 -> Field (HandlerT site IO) [a]
checkboxesField' ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val isReq -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            [whamlet|
                <span ##{theId}>
                    $forall opt <- opts
                        <label>
                            <br>
                            <input type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
                            #{optionDisplay opt}
                |]
    }