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
pdxPair = PdxPair <$> ((,,) 
    <$> (uStringLiteral <|> intLiteral) 
    <*> (ws *> opLiteral <* ws) 
    <*> pdxValue
    )

pdxValue :: Parser PdxValue
pdxValue = pdxArray <|> pdxHsvArray <|> pdxPair <|> pdxBool <|> pdxNumber <|> pdxQString <|> pdxUString

stripComments :: String -> String
stripComments = concatMap (\l -> takeWhile (/= '#') l ++ "\n") . lines

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
    content = concatMap (\x -> indent (n+1) ++ toPdxScriptIndent (n+1) x ++ "\n")

setEntryMode :: String -> PdxValue -> PdxValue
setEntryMode mode (PdxArray xs) = PdxArray (map prependToKey xs)
  where
    prependToKey :: PdxValue -> PdxValue
    prependToKey (PdxPair (key, op, val)) = PdxPair (mode ++ key, op, val)
    prependToKey other = other
setEntryMode mode other = other

isDatabase :: FilePath -> Bool
isDatabase dstDir = 
  let normalizedDst = map (\c -> if c == '\\' then '/' else c) dstDir
  in any (`isSuffixOf` normalizedDst) databasePaths
    where databasePaths = 
            [ "common/acceptance_statuses"
            , "common/ai_strategies"
            , "common/amendments"
            , "common/battle_conditions"
            , "common/building_groups"
            , "common/buildings"
            , "common/buy_packages"
            , "common/character_interactions"
            , "common/character_templates"
            , "common/character_traits"
            , "common/cohesion_levels"
            , "common/combat_unit_groups"
            , "common/combat_unit_types"
            , "common/combat_unit_experience_levels"
            , "common/commander_orders"
            , "common/company_charter_types"
            , "common/company_types"
            , "common/country_creation"
            , "common/country_definitions"
            , "common/country_formation"
            , "common/country_ranks"
            , "common/country_types"
            , "common/culture_graphics"
            , "common/cultures"
            , "common/decisions"
            , "common/decrees"
            , "common/diplomatic_actions"
            , "common/diplomatic_catalyst_categories"
            , "common/diplomatic_catalysts"
            , "common/diplomatic_plays"
            , "common/discrimination_trait_groups"
            , "common/discrimination_traits"
            , "common/dna_data"
            , "common/dynamic_company_names"
            , "common/dynamic_country_names"
            , "common/dynamic_country_map_colors"
            , "common/dynamic_treaty_names"
            , "common/technology"
            , "common/flag_definitions"
            , "common/game_concepts"
            , "common/genes"
            , "common/geographic_regions"
            , "common/goods"
            , "common/government_types"
            , "common/harvest_condition_types"
            , "common/ideologies"
            , "common/institutions"
            , "common/interest_group_traits"
            , "common/interest_groups"
            , "common/journal_entry_groups"
            , "common/journal_entries"
            , "common/law_groups"
            , "common/laws"
            , "common/legitimacy_levels"
            , "common/liberty_desire_levels"
            , "common/military_formation_flags"
            , "common/mobilization_option_groups"
            , "common/mobilization_options"
            , "common/objective_subgoal_categories"
            , "common/objective_subgoals"
            , "common/objectives"
            , "common/parties"
            , "common/political_lobby_appeasement"
            , "common/political_lobbies"
            , "common/political_movement_categories"
            , "common/political_movement_pop_support"
            , "common/political_movements"
            , "common/pop_needs"
            , "common/pop_types"
            , "common/power_bloc_coa_pieces"
            , "common/power_bloc_identities"
            , "common/power_bloc_map_textures"
            , "common/power_bloc_names"
            , "common/power_bloc_principle_groups"
            , "common/power_bloc_principles"
            , "common/prestige_goods"
            , "common/production_method_groups"
            , "common/production_methods"
            , "common/proposal_types"
            , "common/religions"
            , "common/social_classes"
            , "common/social_hierarchies"
            , "common/state_traits"
            , "common/strategic_regions"
            , "common/subject_types"
            , "common/terrain_manipulators"
            , "common/terrain"
            , "common/themes"
            , "common/tutorial_lessons"
            , "common/tutorial_lesson_chains"
            , "common/labels"
            , "common/war_goal_types"
            , "common/alert_groups"
            , "common/alert_types"
            , "common/commander_ranks"
            , "common/scripted_buttons"
            , "common/scripted_progress_bars"
            , "common/treaty_articles"
            , "common/modifier_type_definitions"
            , "common/ethnicities"
            , "common/script_values"
            , "common/scripted_guis"
            , "common/scripted_lists"
            , "common/scripted_modifiers"
            , "common/achievements"
            , "gfx/map/city_data/city_building_vfx"
            , "gfx/map/fleet_dioramas"
            , "gfx/map/fleet_entities"
            , "gfx/map/army_dioramas"
            , "gfx/map/front_entities"
            , "gfx/portraits/accessories"
            , "gfx/portraits/portrait_modifiers"
            --, "map_data/state_regions" -- doesn't work even though its in the list from the dev diary idk
            , "sound/persistent_objects"
            , "music"
            , "notifications"
            , "gui_animations"
            , "modifier_icons"
            ]

processDirectory :: [String] -> (PdxValue -> PdxValue) -> FilePath -> FilePath -> IO ()
processDirectory excludePrefixes transform srcDir dstDir = do
  createDirectoryIfMissing True dstDir
  entries <- listDirectory srcDir
  forM_ entries $ \entry -> do
    let srcPath = srcDir </> entry
    let dstPath = dstDir </> "bem_" ++ entry
    isFile <- doesFileExist srcPath
    let isBlacklisted = any (`isPrefixOf` entry) excludePrefixes
    when (isFile && takeExtension entry == ".txt" && not isBlacklisted) $ do
      result <- parseFile srcPath
      case result of
        Just parsed -> do
          let finalTransform = if isDatabase dstDir 
                               then setEntryMode "REPLACE:" . transform
                               else transform
          writeFile dstPath $ toPdxScript $ finalTransform parsed
        Nothing -> putStrLn $ "Failed to parse: " ++ srcPath
