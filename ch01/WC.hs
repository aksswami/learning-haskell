main = interact wordCount
  where wordCount input = show (foldr (\x acc -> acc + length (words x)) 0 (lines input)) ++ "\n"
