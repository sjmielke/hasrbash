import Text.XML.Light
--import Network.HTTP
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





fetchRSS, fetchRSS' :: IO String
fetchRSS = {- do
           url <- readFile ".conf"
           response <- simpleHTTP (getRequest url)
           body <- getResponseBody response
           return body -} return ""
fetchRSS' = readFile "data/bash.xml"

parseQuotes :: String -> [String]
parseQuotes = map simplifyDescriptions
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
    simplifyDescriptions = decodeString
                         . unescapeEntities
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


main = do quotes <- parseQuotes <$> fetchRSS'
          mapM_ putStrLn $ take 5 $ onlySingleQuotes quotes
  where
    onlySingleQuotes :: [String] -> [String]
    onlySingleQuotes = filter (\s -> not ('\n' `elem` s))
