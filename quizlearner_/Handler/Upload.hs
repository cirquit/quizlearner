module Handler.Upload where

import Import

form :: Html -> MForm Handler (FormResult FileInfo, Widget)
form = renderDivs $ fileAFormReq "File"

getUploadR :: Handler Html
getUploadR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout [whamlet|$newline never
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit>
|]

postUploadR :: Handler Html
postUploadR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        [whamlet|$newline never
$maybe file <- msubmission
    <p>File received: #{fileName file}
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit>
|]