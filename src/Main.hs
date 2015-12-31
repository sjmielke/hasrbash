import Text.XML.Light
import Network.HTTP
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Codec.Binary.UTF8.String (decodeString)
import Debug.Trace
import Text.Printf (printf)


newtype Quote = Quote [QPart] deriving (Eq, Show)
data QPart = QSpeech { getSpeaker :: String
                     , getAttribute :: String
                     , getSpoken :: String
                     }
           | QOther  { getOther :: String }
             deriving (Eq, Show)

isQSpeech :: QPart -> Bool
isQSpeech (QSpeech _ _ _) = True
isQSpeech _ = False

ppQuote :: Quote -> String
ppQuote (Quote qps) = replicate 30 '-' ++ "\n" ++ unlines (map ppQp qps)
  where
    ppQp (QOther s) = s
    ppQp (QSpeech speaker attribute spoken)
      = printf ("%" ++ show (2 + maxSpeakerLength) ++ "s %s")
               ("<" ++ annotateSpeaker speaker attribute ++ ">")
               spoken
    annotateSpeaker s "" = s
    annotateSpeaker s a = s ++ " {" ++ a ++ "}"
    maxSpeakerLength = maximum
                     $ map (\(QSpeech s a _) -> length $ annotateSpeaker s a)
                     $ filter isQSpeech qps



fetchRSS, fetchRSS' :: IO String
fetchRSS = do
           url <- readFile ".conf"
           response <- simpleHTTP (getRequest url)
           body <- getResponseBody response
           return body
fetchRSS' = readFile "data/bash.xml"

parseQuotes :: String -> [Quote]
parseQuotes = map (parseQuote . simplifyDescriptions)
            . tail -- first one is the description of the site
            . findElements (unqual "description")
            . getRSSChannel
            . parseXML
  where
    getRSSChannel :: [Content] -> Element
    getRSSChannel = fromJust . findElement (unqual "channel")
                  . head . tail -- second element is the channel
                  . onlyElems
    simplifyDescriptions :: Element -> String
    simplifyDescriptions = unescapeEntities
                         . concatMap cdData
                         . onlyText
                         . elContent
    unescapeEntities :: String -> String
    unescapeEntities [] = []
    unescapeEntities ('&':xs)
      = let (b, a) = break (== ';') xs in
          case (lookupEntity b, a) of
            (Just c, ';':as) ->  c ++ unescapeEntities as
            _                -> '&' : unescapeEntities xs
    unescapeEntities (x:xs) = x : unescapeEntities xs
    parseQuote :: String -> Quote
    parseQuote = Quote . map parseQuoteLine . lines

parseQuoteLine :: String -> QPart
parseQuoteLine l = parsed
  where
    parsed = case parse quoteDefinition "" l of
               Left _ -> QOther l
               Right s@(QSpeech _ _ _) -> s
    quoteDefinition, prefixedLine, dashStyleLine
      :: Parser QPart -- Always QSpeech or a failure!
    quoteDefinition = try prefixedLine <|> dashStyleLine
    prefixedLine
      = do
        fullspeaker <- try ircStyleStart <|> colonStyleStart
        spoken <- many anyChar
        let (speaker, attribute) = interpretSpeaker fullspeaker
        when (null (trim spoken) || null (trim speaker)) $ unexpected "we were wrong"
        return $ QSpeech speaker attribute (unquote $ trim spoken)
    ircStyleStart, colonStyleStart :: Parser String
    ircStyleStart
      = between (char '<') (char '>') (many $ noneOf ">")
    colonStyleStart
      = do
        fullspeaker <- many letter -- Obviously breaks for some names (and doesn't support attributes)
        string ": "
        return fullspeaker
    dashStyleLine
      = do
        spokenParts <- many $ try $ anyChar `manyTill` trySpacedDash
        let spoken = intercalate "-" spokenParts -- TODO: if spoken is empty we dont have the dash-syntax but a »speaker: spoken«-style syntax. this is highly ambiguous and we likely won't get it correct all the time anyway. so look at the data and decide on some precedence :) [or just edit the fucking db to whatever is more prevalent lately]
        fullspeaker <- many anyChar
        let (speaker, attribute) = interpretSpeaker fullspeaker
        when (null (trim spoken) || null (trim speaker)) $ unexpected "we were wrong"
        return $ QSpeech speaker attribute (unquote $ trim spoken)
      where
        dash = char '-' <|> char '\8211'
        trySpacedDash = (try $ dash >> space) <|> (try $ space >> dash)
    interpretSpeaker :: String -> (String, String)
    interpretSpeaker fullspeaker
      | last fullspeaker == ')' = (trim parenSpeaker, trim $ init parenAttribute)
      | otherwise = (trim commaSpeaker, intercalate ", " $ map trim commaAttributes)
      where
        (parenSpeaker : parenAttribute : []) = splitOn "(" fullspeaker
        (commaSpeaker : commaAttributes) = splitOn "," fullspeaker
    unquote "" = error $ "Tried unquoting the »spoken« of: " ++ l
    unquote xs
      -- TODO: support different quotes?
      | head xs == '"' && last xs == '"' = trim $ init $ tail xs
      | otherwise = xs
    trim = unwords . words




main = do quotes <- parseQuotes <$> fetchRSS'
          let lookAtQuotes = filter (not . isSane) quotes
          when (not $ all isSane lookAtQuotes) $ print "Insanity!"
          mapM_ (putStr . ppQuote) lookAtQuotes
          print $ length lookAtQuotes
  where
    onlySingleQuotes :: [Quote] -> [Quote]
    onlySingleQuotes = filter (\(Quote q) -> length q == 1)
    onlyMultiQuotes :: [Quote] -> [Quote]
    onlyMultiQuotes = filter (\(Quote q) -> length q /= 1)
    isSane :: Quote -> Bool
    isSane (Quote qps) = any isQSpeech qps
