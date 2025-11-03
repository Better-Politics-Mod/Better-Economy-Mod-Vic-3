{-# LANGUAGE OverloadedStrings #-}
module PdxParserT where
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import System.Directory 
import System.FilePath 
import Text.Printf

data PdxValue = PdxBool Bool
    | PdxNumber Double
    | PdxQString Text
    | PdxUString Text
    | PdxArray [PdxValue]
    | PdxHsvArray [PdxValue]
    | PdxPair (Text, Text, PdxValue)
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: Text -> Maybe (Text, a) }

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
        f input
         | Just (y, ys) <- T.uncons input, y == x = Just (ys, x)
         | otherwise = Nothing

stringP :: Text -> Parser Text
stringP str = Parser $ \input ->
    if str `T.isPrefixOf` input
        then Just (T.drop (T.length str) input, str)
        else Nothing

pdxBool :: Parser PdxValue
pdxBool = f <$> (stringP "yes" <|> stringP "no")
    where f "yes" = PdxBool True
          f "no" = PdxBool False
          f _ = error "this should never happen :clueless:"

spanP :: (Char -> Bool) -> Parser Text
spanP f =
    Parser $ \input ->
        let (token, rest) = T.span f input
        in Just (rest, token)

spanUntilP :: Text -> Parser Text
spanUntilP delim =
    Parser $ \input ->
        let (token, rest) = spanUntil T.empty input
        in Just (rest, token)
            where
                spanUntil acc xs
                    | delim `T.isPrefixOf` xs = (acc, xs)
                    | T.null xs = (acc, T.empty)
                    | otherwise = 
                        let (c, cs) = (T.head xs, T.tail xs)
                        in spanUntil (T.snoc acc c) cs

notNull :: Parser Text -> Parser Text
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if T.null xs
            then Nothing
            else Just (input', xs)

pdxNumber :: Parser PdxValue
pdxNumber =
    Parser (runParser (notNull (spanP (\ c -> isDigit c || c == '.' || c == '-')))
   >=>
     (\ (rest, ds)
        -> if T.length (T.filter (== '.') ds) <= 1 then
               Just (rest, PdxNumber (read $ T.unpack ds))
           else
               Nothing))

qStringLiteral :: Parser Text
qStringLiteral = spanP (/= '"')

pdxQString :: Parser PdxValue
pdxQString = PdxQString <$> (charP '"' *> qStringLiteral <* charP '"')

uStringLiteral :: Parser Text
uStringLiteral = notNull $ spanP (\c -> not (isSpace c) && c `notElem` ("}=" :: String))

pdxUString :: Parser PdxValue
pdxUString = PdxUString <$> (ws *> uStringLiteral <* ws)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser Text
ws = spanP isSpace

pdxArray :: Parser PdxValue
pdxArray = PdxArray <$> (charP '{' *> ws *> elements <* ws <* charP '}')
    where elements = sepBy ws pdxValue

pdxHsvArray :: Parser PdxValue
pdxHsvArray = PdxHsvArray <$> (stringP "hsv{" *> ws *> elements <* ws <* charP '}')
    where elements = sepBy ws pdxValue

intLiteral :: Parser Text
intLiteral = notNull $ spanP isDigit

opLiteral :: Parser Text
opLiteral = notNull $ spanP (`elem` ("<>=" :: String))

pdxPair :: Parser PdxValue
pdxPair = PdxPair <$> ((\key op value -> (key, op, value)) 
    <$> (uStringLiteral <|> intLiteral) 
    <*> (ws *> opLiteral <* ws) 
    <*> pdxValue
    )

pdxValue :: Parser PdxValue
pdxValue = pdxArray <|> pdxHsvArray <|> pdxPair <|> pdxBool <|> pdxNumber <|> pdxQString <|> pdxUString

stripComments :: Text -> Text
stripComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines

unBOM :: Text -> Text
unBOM text
    | Just ('\xFEFF', rest) <- T.uncons text = rest
    | otherwise = text

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    content <- TIO.readFile fileName
    let processed = T.cons '{' $ T.snoc (stripComments $ unBOM content) '}'
    return $ snd <$> runParser parser processed

class ToPdxScript a where
    toPdxScript :: a -> Text

instance ToPdxScript PdxValue where
    toPdxScript :: PdxValue -> Text
    toPdxScript (PdxArray xs) = T.concat $ map (\x -> toPdxScriptIndent 0 x <> "\n") xs
    toPdxScript v = toPdxScriptIndent 0 v

toPdxScriptIndent :: Int -> PdxValue -> Text
toPdxScriptIndent n v = case v of
    PdxBool b -> if b then "yes" else "no"
    PdxNumber num -> T.pack $ reverse . dropWhile (== '.') . dropWhile (== '0') . reverse $ printf "%.3f" num
    PdxQString s -> T.pack (show s)
    PdxUString s -> s
    PdxArray xs -> "{\n" <> content xs <> indent n <> "}"
    PdxHsvArray xs -> "hsv{\n" <> content xs <> indent n <> "}"
    PdxPair (k,op,val) -> k <> " " <> op <> " " <> toPdxScriptIndent n val
  where
    indent :: Int -> Text
    indent n' = T.replicate n' "\t"
    content :: [PdxValue] -> Text
    content xs = T.concat $ map (\x -> indent (n+1) <> toPdxScriptIndent (n+1) x <> "\n") xs

processDirectory :: [String] -> (a -> PdxValue) -> Parser a -> FilePath -> FilePath -> IO ()
processDirectory excludePrefixes transform parser srcDir dstDir = do
  createDirectoryIfMissing True dstDir
  entries <- listDirectory srcDir
  forM_ entries $ \entry -> do
    let srcPath = srcDir </> entry
    let dstPath = dstDir </> entry
    isFile <- doesFileExist srcPath
    let isBlacklisted = any (`L.isPrefixOf` entry) excludePrefixes
    when (isFile && takeExtension entry == ".txt" && not isBlacklisted) $ do
      result <- parseFile srcPath parser
      case result of
        Just parsed -> do
          TIO.writeFile dstPath $ toPdxScript $ transform parsed
        Nothing -> putStrLn $ "Failed to parse: " ++ srcPath