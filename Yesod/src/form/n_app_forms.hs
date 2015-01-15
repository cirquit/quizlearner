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
/          HomeR GET
|]
-- /car/#Int  CarR

--data Car = Car { carModel :: Text
--               , carYear  :: Int
--               , carColor :: Maybe Text
--               }
--   deriving Show

--carAForm :: Int -> AForm Handler Car
--carAForm x = Car
--    <$> areq textField "Model"  (Just (T.pack (show x)))
--    <*> areq intField  "Year"   (Just 1996)
--    <*> aopt textField "Color"  Nothing
--   -- <*> areq hiddenField "This is a question!" (Just (T.pack "This is a question!")) -- pure doesnt show anything
--   -- <*> areq checkBoxField "Some answer" (Just True)
--
--intAForm :: AForm Handler Int
--intAForm = areq intField "Bitte geben sie eine positive Zahl ein!" Nothing
--
--carForm :: Html -> MForm Handler (FormResult Car, Widget)
--carForm = renderBootstrap $ carAForm 1
--
--nCarAForm :: Int -> AForm Handler [Car]
--nCarAForm 1 = (\x -> [x]) <$> carAForm 1
--nCarAForm n = (:) <$> carAForm n <*> nCarAForm (n-1)


t1, t2, t3, t4 :: (String,Bool)
t1 = ("t1 - false1", False)
t2 = ("t2 - false2", False)
t3 = ("t3 - true3", True)
t4 = ("t4 - false4", False)


listEditMForm :: [(String,Bool)]-> MForm Handler (FormResult [(FormResult Bool, FormResult Text)], Widget)
listEditMForm xs = do
    ifields <- forM xs (\(s,i) -> mreq intField  (fromString s) (Just i))
    tfields <- forM xs (\(s,i) -> mreq textField (fromString s) (Just $ pack s))
    let (iresults,iviews) = unzip ifields
    let (tresults,tviews) = unzip tfields
    let results = zip iresults tresults
    let views   = zip iviews tviews
    let widget = [whamlet|
        <h1>Multi Field Form
        $forall (iv,tv) <- views
          Field #
          #{fvLabel iv}: #
          ^{fvInput tv} #
          ^{fvInput iv}
          <input type=submit value="Testing mForms">
      |]
    return ((FormSuccess results), widget)





--linkWidget :: Int -> Widget
--linkWidget count = toWidget [hamlet| <a href=@{CarR count}> Klick mich!|]

getHomeR :: Handler Html
getHomeR = do
        ((res, widget), enctype) <- runFormGet $ listEditMForm [t1, t2, t3, t4]
        defaultLayout [whamlet|
              <p>Result = #{show res}
              <form enctype=#{enctype}>
                  ^{widget}

                      |]

        --((result, widget), enctype) <- runFormGet $ renderBootstrap intAForm
        --case result of
        --    FormSuccess int -> if int > 0 then defaultLayout [whamlet|<p> Hallo!
        --                                                               ^{linkWidget int}
        --                                                     |]
        --                                  else defaultLayout [whamlet|<form method=get action=@{HomeR} enctype=#{enctype}>
        --                                                                  ^{widget}
        --                                                                  <button>Submit
        --                                                     |]
        --    FormMissing     -> defaultLayout [whamlet|<form method=get action=@{HomeR} enctype=#{enctype}>
        --                                                                  ^{widget}
        --                                                                  <button>Submit
        --                                                     |]
        --    _               -> defaultLayout [whamlet|<form method=get action=@{HomeR} enctype=#{enctype}>
        --                                                                  ^{widget}
        --                                                                  <button>Submit
        --                                                     |]

--handleCarR :: Int -> Handler Html
--handleCarR count = do
--  ((result,widget), enctype) <- runFormPost $ renderBootstrap $ nCarAForm count
--  case result of
--    FormMissing ->  defaultLayout $ do
--      setTitle "Form Demo"
--      [whamlet|
--            <h2>Form Demo
--            <form method=post action=@{CarR count} enctype=#{enctype}>
--              ^{widget}
--              <button>Submit
--      |]
--
--    FormSuccess car -> defaultLayout $ do
--      setTitle "Form Auswerten"
--      [whamlet|
--            <h2>Car received:
--            <p>#{show car}
--            <p>
--              <a href=@{HomeR}>Zurück
--      |]
--    _               -> defaultLayout
--      [whamlet|
--            <h2>Fehler!
--            <p>Bitte nochmal eingeben:
--            <form method=post action=@{CarR count} enctype=#{enctype}>
--              ^{widget}
--              <button>Abschicken
--      |]

