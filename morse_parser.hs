import System.Environment
import Text.ParserCombinators.ReadP
import Data.Semigroup
import Data.List
import Data.List.Split
import Data.Ord

{- a map from characters to morse strings. -}
charToMorse :: [(String, String)]
charToMorse =
    [ ("a", "._")
    , ("b", "_...")
    , ("c", "_._.")
    , ("d", "_..")
    , ("e", ".")
    , ("f", ".._.")
    , ("g", "__.")
    , ("h", "....")
    , ("i", "..")
    , ("j", ".___")
    , ("k", "_._")
    , ("l", "._..")
    , ("m", "__")
    , ("n", "_.")
    , ("o", "___")
    , ("p", ".__.")
    , ("q", "__._")
    , ("r", "._.")
    , ("s", "...")
    , ("t", "_")
    , ("u", ".._")
    , ("v", "..._")
    , ("w", ".__")
    , ("x", "_.._")
    , ("y", "_.__")
    , ("z", "__..")
    , ("1", ".____")
    , ("2", "..___")
    , ("3", "...__")
    , ("4", "...._")
    , ("5", ".....")
    , ("6", "_....")
    , ("7", "__...")
    , ("8", "___..")
    , ("9", "____.")
    , ("0", "_____")
    , (",", "__..__")
    , (".", "._._._")
    ]

{- a list of all morse strings. -}
allMorse :: [String]
allMorse = map snd charToMorse

{- tries to parse character out of morse string. -}
parseOneMorse :: String -> ReadP String
parseOneMorse =
    flip performLookup charToMorse
    where
        performLookup s (pair:pairs)
            | (snd pair) == s = (string s) >> return (fst pair)
            | otherwise       = performLookup s pairs

{- tries every single possible morse parse and returns all successful parses. -}
parseMorse :: ReadP String
parseMorse =  foldr1 (+++) $ map parseOneMorse allMorse

{- gets shortest string in list of strings. -}
shortest :: [String] -> String
shortest = minimumBy (comparing length)

{- flip compose to make reading main easier. -}
(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

main :: IO ()
main = getContents >>=
       splitOn " / "                                  >>> -- split individual words
       map (map (\c -> if c == '-' then '_'; else c)) >>> -- sanitize input
       map words                                      >>> -- further sanitize
       map (map (readP_to_S parseMorse))              >>> -- parse morse code
       map (map (fst . last))                         >>> -- only take full parse
       map (foldr1 (++))                              >>> -- put words together
       unwords                                        >>> -- crunch list to string
       putStrLn
