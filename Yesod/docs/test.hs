module Mean where

import Data.List (foldl')

mean :: (Integral a, Fractional b) => [a] -> b
mean xs = (fromIntegral $ fst acc) / (fromIntegral $ snd acc)
    where acc = foldl' (\(n,m) x -> (n+x, m+1)) (0,0) xs



mean' :: (Integral a, Fractional b) => [a] -> b
mean' xs = fst result
 where result = acc (\x -> (fromIntegral x) / (fromIntegral $ snd result)) xs
       --acc :: (Double -> Double) -> [Double] -> (Double, Double)
       acc f = foldr (\x (a,b) -> ((f x) + a, b+1)) (0,0)


-- decrementByMinium :: [Double] -> [Double]
-- decrementByMinium xs =  fst result
--  where result = acc (\x -> x - (snd result)) xs
--       acc ::  (Double -> Double) -> [Double] -> ([Double], Double)
--       acc f (x:xs) = foldr (\y (q,p) -> ((f y : q), min y p)) ([],x) (x:xs)



--mean :: (Integral a, Fractional b) => [a] -> b
--mean xs = (fromIntegral (sum xs )) / fromIntegral (length xs)