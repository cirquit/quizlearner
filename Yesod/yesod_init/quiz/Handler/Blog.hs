module Handler.Blog (getBlogR, postBlogR) where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
--instance YesodNic App
instance YesodNic App

entryForm :: Form Article
entryForm = renderDivs $ Article
  <$> areq textField "Title" Nothing
  <*> areq nicHtmlField "Content" Nothing

getBlogR :: Handler Html
getBlogR = do 
  articles <- runDB $ selectList [] [Desc ArticleTitle]
  (articleWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    $(widgetFile "articles")

postBlogR :: Handler Html
postBlogR = do
    ((res,articleWidget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")