import Text.XML.Light
import Network.HTTP
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (liftM)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Codec.Binary.UTF8.String (decodeString)

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

main = readFile ".conf"
       >>= fetch
       >>= mapM_ putStrLn . getQuotes . parseXML
