module SplitLines where

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = pre : case suf of
                        ('\r':'\n':rest) -> splitLines rest
                        ('\r':rest)      -> splitLines rest
                        ('\n':rest)      -> splitLines rest
                        _                -> []
                where (pre, suf) = break isLineTerminator cs

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\n' || c == '\r'

fixLines :: String -> String
fixLines input = unlines (splitLines input)
