import Data.List
 
--main = print (entropy "0000111222")
 
entropy :: (Ord a, Floating b) => [a] -> b
entropy = sum . map lg . fq . map genericLength . group . sort 
  where lg b = -b * logBase 2 b
        fq b = let sb = sum b in map (/ sb) b
