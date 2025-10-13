module Main where
import Data.Char
import Control.Applicative
import Control.Monad
import System.Environment (getArgs)

data PdxValue = PdxBool Bool
    | PdxNumber Double
    | PdxString String
    | PdxArray [PdxValue]
    | PdxPair (String, PdxValue)
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Just (input, x)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
         | y == x = Just (ys, x)
         | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

pdxBool :: Parser PdxValue
pdxBool = f <$> (stringP "yes" <|> stringP "no")
    where f "yes" = PdxBool True
          f "no" = PdxBool False

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
            in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

pdxNumber :: Parser PdxValue
pdxNumber =
    Parser (runParser (notNull (spanP (\ c -> isDigit c || c == '.' || c == '-')))
   >=>
     (\ (rest, ds)
        -> if length (filter (== '.') ds) <= 1 then
               Just (rest, PdxNumber (read ds))
           else
               Nothing))

qStringLiteral :: Parser String
qStringLiteral = spanP (/= '"')

pdxQString :: Parser PdxValue
pdxQString = PdxString <$> (charP '"' *> qStringLiteral <* charP '"')

uStringLiteral :: Parser String
uStringLiteral = spanP (\c -> not (isSpace c) && c /= '"' && c /= '=' && c /= '}')

pdxUString :: Parser PdxValue
pdxUString = Parser (runParser (ws *> uStringLiteral <* ws)
    >=>
        (\ (rest, ds)
            -> if ds == "" then
                Nothing
            else
                Just (rest, PdxString ds)))

pdxString :: Parser PdxValue
pdxString = pdxQString <|> pdxUString

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP isSpace

pdxArray :: Parser PdxValue
pdxArray = PdxArray <$> (charP '{' *> ws *> elements <* ws <* charP '}')
    where elements = sepBy (spanP isSpace) pdxValue

intLiteral :: Parser String
intLiteral = notNull (spanP isDigit)

pdxPair :: Parser PdxValue
pdxPair = PdxPair <$> pair 
    where
        pair = (\key _ value -> (key, value)) <$> (uStringLiteral <|> intLiteral)
                     <*> (ws *> charP '=' <* ws)
                     <*> pdxValue

pdxValue :: Parser PdxValue
pdxValue = pdxArray <|> pdxPair <|> pdxBool <|> pdxNumber <|> pdxString

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    let brIn = "{" ++ input ++ "}"
    return (snd <$> runParser parser brIn)

main :: IO ()
main = do
    args <- getArgs
    let arg = head args
    result <- parseFile arg pdxValue
    print result