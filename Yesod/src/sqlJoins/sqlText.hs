

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    firstName String
    lastName String
    age Int
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
|]

-- Nothing special here
instance Yesod PersistTest

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- List all people in the database
getHomeR :: Handler Html
getHomeR = do
    people <- runDB $ selectList [] [Asc PersonAge]
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{PersonR personid}>#{personFirstName person}
        |]

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getPersonR :: PersonId -> Handler String
getPersonR personId = do
    person <- runDB $ get404 personId
    return $ show person

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ Person "Michael" "Snoyman" 26
    warp 3000 $ PersistTest pool