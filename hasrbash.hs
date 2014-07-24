import Text.XML.Light
import Network.HTTP
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (liftM2)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Codec.Binary.UTF8.String (decodeString)
import Text.Parsec
import Text.Parsec.Text
import Data.Text (pack)
import Data.Either (rights)
import Data.List (sort, group, sortBy)
import Data.Ord (comparing)

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
myParse = parse quoteDefinition "(rss feed)" . pack . reverse

quoteDefinition :: GenParser state (String, String)
quoteDefinition = do author <- many1 (noneOf " - \"")
                     try (string " - \"")
                     text <- many1 (noneOf "\"")
                     char '"'
                     return (reverse author, reverse text)

--------------------------------------------------------------------------------

main = do rss <- readFile ".conf" >>= fetch
          let quotes = rights . map myParse . onlySimpleQuotes . getQuotes . parseXML $ rss
          mapM_ print quotes
          print $ length quotes
          print $ reverse . sortBy (comparing snd) . map (liftM2 (,) head length) . group . sort $ map fst quotes
