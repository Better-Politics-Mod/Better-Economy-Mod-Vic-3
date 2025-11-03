module PdxParser where
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad
import System.Directory 
import System.FilePath 
import Text.Printf

data PdxValue = PdxBool Bool
    | PdxNumber Double
    | PdxQString String
    | PdxUString String
    | PdxArray [PdxValue]
    | PdxHsvArray [PdxValue]
    | PdxPair (String, String, PdxValue)
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

instance Num PdxValue where
    (PdxNumber a) + (PdxNumber b) = PdxNumber (a + b)
    (PdxNumber a) - (PdxNumber b) = PdxNumber (a - b)
    (PdxNumber a) * (PdxNumber b) = PdxNumber (a * b)
    abs (PdxNumber a) = PdxNumber (abs a)
    signum (PdxNumber a) = PdxNumber (signum a)
    fromInteger n = PdxNumber (fromInteger n)
    negate (PdxNumber a) = PdxNumber (negate a)

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
          f _ = error "this should never happen :clueless:"

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
        in Just (rest, token)

spanUntilP :: String -> Parser String
spanUntilP delim =
    Parser $ \input ->
        let (token, rest) = spanUntil [] input
        in Just (rest, token)
            where
                spanUntil acc xs
                    | delim `isPrefixOf` xs = (reverse acc, xs)
                    | otherwise = case xs of
                        [] -> (reverse acc, "")
                        (c:cs) -> spanUntil (c:acc) cs

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

pdxNumber :: Parser PdxValue
pdxNumber = Parser $ \input -> do
    (rest, numStr) <- runParser (notNull $ spanP (\c -> isDigit c || c == '.' || c == '-')) input
    guard $ length (filter (== '.') numStr) <= 1
    Just (rest, PdxNumber (read numStr))

qStringLiteral :: Parser String
qStringLiteral = spanP (/= '"')

pdxQString :: Parser PdxValue
pdxQString = PdxQString <$> (charP '"' *> qStringLiteral <* charP '"')

uStringLiteral :: Parser String
uStringLiteral = notNull $ spanP (\c -> not (isSpace c) && c `notElem` "}=<>")

pdxUString :: Parser PdxValue
pdxUString = PdxUString <$> (ws *> uStringLiteral <* ws)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP isSpace

pdxArray :: Parser PdxValue
pdxArray = PdxArray <$> (charP '{' *> ws *> elements <* ws <* charP '}')
    where elements = sepBy ws pdxValue

pdxHsvArray :: Parser PdxValue
pdxHsvArray = PdxHsvArray <$> (stringP "hsv{" *> ws *> elements <* ws <* charP '}')
    where elements = sepBy ws pdxValue

intLiteral :: Parser String
intLiteral = notNull $ spanP isDigit

opLiteral :: Parser String
opLiteral = notNull $ spanP (`elem` "=<>!")

pdxPair :: Parser PdxValue
pdxPair = PdxPair <$> ((\key op value -> (key, op, value)) 
    <$> (uStringLiteral <|> intLiteral) 
    <*> (ws *> opLiteral <* ws) 
    <*> pdxValue
    )

pdxValue :: Parser PdxValue
pdxValue = pdxArray <|> pdxHsvArray <|> pdxPair <|> pdxBool <|> pdxNumber <|> pdxQString <|> pdxUString

stripComments :: String -> String
stripComments = concat . map (\l -> takeWhile (/= '#') l ++ "\n") . lines

unBOM :: String -> String
unBOM ('\xFEFF':xs) = xs
unBOM x = x

parseFile :: FilePath -> IO (Maybe PdxValue)
parseFile fileName = (snd <$>) . runParser pdxValue . ("{" ++) . (++ "}") . stripComments . unBOM <$> readFile fileName

class ToPdxScript a where
    toPdxScript :: a -> String

instance ToPdxScript PdxValue where
    toPdxScript :: PdxValue -> String
    toPdxScript (PdxArray xs) = concatMap (\x -> toPdxScriptIndent 0 x ++ "\n") xs
    toPdxScript v = toPdxScriptIndent 0 v

toPdxScriptIndent :: Int -> PdxValue -> String
toPdxScriptIndent n v = case v of
    PdxBool b -> if b then "yes" else "no"
    PdxNumber num -> reverse . dropWhile (== '.') . dropWhile (== '0') . reverse $ printf "%.3f" num
    PdxQString s -> show s
    PdxUString s -> s
    PdxArray xs -> "{\n" ++ content xs ++ indent n ++ "}"
    PdxHsvArray xs -> "hsv{ " ++ unwords (map (toPdxScriptIndent n) xs) ++ " }"
    PdxPair (k,op,val) -> k ++ ' ' : op ++ ' ' : toPdxScriptIndent n val
  where
    indent :: Int -> String
    indent n' = replicate n' '\t'
    content :: [PdxValue] -> String
    content xs = concatMap (\x -> indent (n+1) ++ toPdxScriptIndent (n+1) x ++ "\n") xs

processDirectory :: [String] -> (PdxValue -> PdxValue) -> FilePath -> FilePath -> IO ()
processDirectory excludePrefixes transform srcDir dstDir = do
  createDirectoryIfMissing True dstDir
  entries <- listDirectory srcDir
  forM_ entries $ \entry -> do
    let srcPath = srcDir </> entry
    let dstPath = dstDir </> entry
    isFile <- doesFileExist srcPath
    let isBlacklisted = any (`isPrefixOf` entry) excludePrefixes
    when (isFile && takeExtension entry == ".txt" && not isBlacklisted) $ do
      result <- parseFile srcPath
      case result of
        Just parsed -> do
          writeFile dstPath $ toPdxScript $ transform parsed
        Nothing -> putStrLn $ "Failed to parse: " ++ srcPath