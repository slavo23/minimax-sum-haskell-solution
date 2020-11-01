module Main where


nthTail :: Int -> [a] -> [a]
nthTail _ [] = []
nthTail 0 xs = tail xs
nthTail n xs = tail $ nthTail (n - 1) xs


subsequentSums :: (Num a, Ord a) => [a] -> (a, a)
subsequentSums [] = (0, 0)
subsequentSums xs = (maximum sums, minimum sums)
    where sums    = map sum (subseqs xs ++ subseqs (reverse xs))
          subseqs = go 1  
          go n xs' | n <= length xs' - 1 = xt : go (n + 1) xs'
                     | otherwise = []
                     where xt = take n xs' ++ nthTail n xs' 


main :: IO ()
main = print $ subsequentSums [1..1000]
