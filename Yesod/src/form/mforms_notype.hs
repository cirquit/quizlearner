{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

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
             --  , identity ::  Text
                qcontent :: Text
               , answerList :: [(Text,Bool)]
             --  , maxScore :: Int
               } deriving Show

data Answer = Answer {
              -- , identity :: Text
                acontent :: Text
         --      , isCorrect :: Bool
               , isChecked :: Bool
              -- , hint :: Text
               } deriving Show

q1,q2 :: Question
q2 = Question {qcontent="Wieviel ist 3+4?", answerList= q2_answers }--q2_answers}

q1_answers, q2_answers :: [(Text, Bool)]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]
q2_answers = [q2_a1,q2_a2,q2_a3,q2_a4]



--q1_a1, q1_a2, q1_a3,q1_a4,q2_a1, q2_a2, q2_a3, q2_a4 :: Answer
--q1_a1 = Answer {acontent="1", isChecked=False}
--q1_a2 = Answer {acontent="2", isChecked=True }
--q1_a3 = Answer {acontent="3", isChecked=False}
--q1_a4 = Answer {acontent="5", isChecked=True }
--q2_a1 = Answer {acontent="5", isChecked=False}
--q2_a2 = Answer {acontent="9", isChecked=True }
--q2_a3 = Answer {acontent="3", isChecked=False}
--q2_a4 = Answer {acontent="1", isChecked=True }




--listEditMForm ::  [Question]-> Html -> MForm Handler (FormResult [(FormResult [Bool], FormResult Text)], Widget)
--listEditMForm xs extra = do
--    ifields <- forM xs (\(Question _ answerlist ) -> mreq (checkboxesFieldList answerlist)(fromString "testing checkboxesFieldList") (Nothing))
--    tfields <- forM xs (\(Question content _) -> mreq hiddenField (fromString $ "Testing hidden") (Just $ T.pack "Testing hidden"))
--    let (iresults,iviews) = unzip ifields
--    let (tresults,tviews) = unzip tfields
--    let results = zip iresults tresults
--    let views   = zip iviews tviews
--    let widget = [whamlet|
--      ^{extra}
--          <h1>Multi Field Form
--          $forall (iv,tv) <- views
--            <br>
--            Field #
--            #{fvLabel iv}: #
--            ^{fvInput tv} #
--            ^{fvInput iv}
--        <br>
--        <input type=submit value="Testing mForms">
--        <br>
--      |]
--    return ((FormSuccess results), widget)

q1_a1, q1_a2, q1_a3,q1_a4,q2_a1, q2_a2, q2_a3, q2_a4 :: (Text, Bool)

q1_a1 = ("1",False)
q1_a2 = ("2",False)
q1_a3 = ("3",False)
q1_a4 = ("5",True )

q2_a1 = ("5",False)
q2_a2 = ("9",True )
q2_a3 = ("3",False)
q2_a4 = ("1",True )


q1 = Question {qcontent="Wieviel ist 2+3?", answerList= q1_answers }--q1_answers}


listEditMForm :: [Question] -> Html -> MForm Handler (FormResult ([FormResult (Maybe [Bool])]), Widget)
listEditMForm xs extra = do
      check_fields <- forM xs (\(Question content list) -> mopt (checkboxesFieldList list)(fromString $ T.unpack content) (Just $ Just [])
   -- tfields <- forM xs (\(Question content _) -> mreq hiddenField (fromString $ "Testing hidden") (Just $ T.pack "Testing hidden"))
      let (check_results,check_views) = unzip check_fields
    --let (tresults,tviews) = unzip tfields
    --let results = zip iresults tresults
    --let views   = zip iviews tviews
      let widget = [whamlet|
           ^{extra}
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

postHomeR :: Handler Html
postHomeR = do
       ((res,widget), enctype) <- runFormPost $ listEditMForm [q1, q2]
       defaultLayout [whamlet|
              <p> Result: #{show res}
              <a href=@{HomeR}> Get back!
                     |]