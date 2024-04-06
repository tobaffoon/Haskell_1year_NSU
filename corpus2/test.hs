{-# LANGUAGE QuasiQuotes, FlexibleContexts, OverloadedStrings #-}

import Text.Regex.PCRE.Heavy
subw :: [[(a, [a])]] -> [a]
subw x = foldl (\acc l -> acc ++ (map fst l)) [] x