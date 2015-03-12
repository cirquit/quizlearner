import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Attoparsec.Text (inClass)

x = "<!DOCTYPE quiz SYSTEM \"http://localhost:3000/static/dtd/examValidation.dtd\">"

--dtdValidation :: Parser String
dtdValidation = do
    spaceSkip
    string "<!DOCTYPE"
    spaceSkip
    string "quiz"
    spaceSkip
    string "SYSTEM"
    spaceSkip
    string "\"http://localhost:3000/static/dtd/examValidation.dtd\""
    spaceSkip
    char '>'


spaceSkip = many $ satisfy $ inClass [ ' ' , '\t' ]

readExpr :: String -> String
readExpr input = case parse dtdValidation "" input of
    Left err -> "No match: " ++ show err
    Right val -> "Match"


main :: IO()
main = do
  putStrLn "Hello World"