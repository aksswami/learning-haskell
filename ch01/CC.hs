main = interact wordCount
  where wordCount input = show (foldr (\x acc -> acc + characterCount (words x)) 0 (lines input)) ++ "\n"
        characterCount  = foldr (\x acc -> acc + length x) 0
