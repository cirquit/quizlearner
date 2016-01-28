{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
import Yesod.Core
import Yesod.Form
import Yesod.Form.MassInput
import Control.Applicative
import Data.Text (Text, pack)
import Network.Wai.Handler.Warp (run)
import Data.Time (utctDay, getCurrentTime)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

mkYesod "HelloForms" [parseRoutes|
/file FileR GET POST
|]

data HelloForms = HelloForms

instance RenderMessage HelloForms FormMessage where
    renderMessage _ _ = defaultFormMessage

instance Yesod HelloForms where
    approot _ = ""


main = toWaiApp HelloForms >>= run 3000

fileForm = renderTable $ pure (,)
    <*> fileAFormReq "Required file"
    <*> fileAFormOpt "Optional file"

getFileR = do
    ((res, form), enctype) <- runFormPost fileForm
    defaultLayout [whamlet|
<p>Result: #{show res}
<form method=post enctype=#{enctype}>
    <table>
        ^{form}
    <tr>
        <td>
            <input type=submit>
|]

postFileR = getFileR