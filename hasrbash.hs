import Text.XML.Light
import Network.HTTP
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (liftM2)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Codec.Binary.UTF8.String (decodeString)
import Text.Parsec
import Text.Parsec.Text
import Data.Text (pack)
import Data.Either (lefts, rights)
import Data.List (sort, group, sortBy)
import Data.Ord (comparing)
import Data.Char (isSpace)
import Control.Applicative ((<$>), (<*), (*>))

unescapeEntities :: String -> String
unescapeEntities [] = []
unescapeEntities ('&':xs) = let (b, a) = break (== ';') xs in
                              case (lookupEntity b, a) of
                                (Just c, ';':as) ->  c ++ unescapeEntities as
                                _                -> '&' : unescapeEntities xs
unescapeEntities (x:xs) = x : unescapeEntities xs

fetch :: String -> IO String
fetch url = do
            response <- simpleHTTP (getRequest url)
            body <- getResponseBody response
            return body

stripToStringLines :: [Content] -> String
stripToStringLines = decodeString . unescapeEntities . concatMap cdData . onlyText

getRSSChannel :: [Content] -> Element
getRSSChannel = fromJust . findElement (unqual "channel") . head . tail . onlyElems

getQuotes :: [Content] -> [String]
getQuotes = map (stripToStringLines . elContent) . findElements (unqual "description") . getRSSChannel

onlySimpleQuotes :: [String] -> [String]
onlySimpleQuotes = filter (\s -> head s == '"' && not ('\n' `elem` s))

--------------------------------------------------------------------------------

myParse :: String -> Either ParseError (String, String)
myParse q = parse quoteDefinition (" »"++q++"« ") . pack . reverse $ q

quoteDefinition :: GenParser state (String, String)
quoteDefinition = (try singleQuote) -- <|> multiQuote

singleQuote :: GenParser state (String, String)
singleQuote = do author <- manyTill anyChar trySpacedDash
                 many space
                 char '"'
                 text <- manyTill anyChar (try $ char '"' >> eof)
                 return (trim . reverse $ author, reverse text)
    where
        trim s = if isSpace $ head s then tail s else s
        dash = char '-' <|> char '\8211'
        trySpacedDash = (try $ dash >> space) <|> (try $ space >> dash) <?> "author separator (dash with at least one space)"

multiQuote :: GenParser state (String, String)
multiQuote = do text <- many1 (noneOf "")
                return ("*Multi", reverse text)

--------------------------------------------------------------------------------

main = do rss <- readFile ".conf" >>= fetch
          let parsed = map myParse . onlySimpleQuotes . getQuotes . parseXML $ rss
          let quotes = rights parsed
          mapM_ print quotes
          print $ length quotes
          print $ reverse . sortBy (comparing snd) . map (liftM2 (,) head length) . group . sort $ map fst quotes
          mapM_ print $ filter ((=="*Multi") . fst) quotes
          mapM_ print $ lefts parsed
