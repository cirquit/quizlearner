module Handler.LineareAlgebra where

import Import
--import Data.List as L ((!!), length)
--import Data.Text
--import System.Random
--import TemporaryLibrary

--type Form a = Html -> MForm Handler (FormResult a, Widget)

--shuffle_answers :: [Answer] -> IO ([Answer])
--shuffle_answers list = do
--    let n = L.length list
--    seed <- getStdRandom (randomR (0, (product [1..n]) - 1))
--    return $ permutations list !! seed
--
--    shuffled_answers <- liftIO $ shuffle_answers $ answer_list snd_q

getLineareAlgebraR :: Handler Html
getLineareAlgebraR = error "GET for LineareAlgebraR has to be defined"


postLineareAlgebraR :: Handler Html
postLineareAlgebraR = error "POST for LineareAlgebraR has to be defined"