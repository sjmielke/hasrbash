import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Codec.Binary.UTF8.String (decodeString)
import Control.Monad (when)
import Data.List (sortOn)
import qualified Data.Map as M

import Debug.Trace

import Quote
import Parse
import GenTable

fetchRSS, fetchRSS' :: IO String
fetchRSS = do
           url <- readFile ".conf"
           response <- simpleHTTP (getRequest url)
           body <- getResponseBody response
           return $ decodeString body
fetchRSS' = readFile "data/bash.xml"


main = do putStrLn "Loading..."
          rss <- fetchRSS
          putStrLn "Parsing..."
          let quotes = parseQuotes rss
          putStrLn "Verifying..."
          when (not $ all (\(Quote qps) -> any isQSpeech qps) quotes) $ error "Insanity!"
          putStrLn "Analyzing..."
          let authors = reverse
                      $ sortOn snd
                      $ M.assocs
                      $ countOccurences
                      $ map speakerExtractor quotes
          putStrLn "Writing..."
          writeFile "/tmp/table.htm" $ getAnalysisAndQuotesAsHtml authors quotes
  where
    speakerExtractor (Quote qps) = getSpeaker $ last $ filter isQSpeech qps
    countOccurences :: Ord a => [a] -> M.Map a Int
    countOccurences = foldl (\m x -> M.insertWith (+) x 1 m) (M.empty :: M.Map a Int)

