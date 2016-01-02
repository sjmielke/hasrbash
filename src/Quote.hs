module Quote where

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
