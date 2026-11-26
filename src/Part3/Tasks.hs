module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (unfoldr, maximumBy)
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = unfoldr (\n -> Just (n, f n)) x

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = digitToInt target
  where
    digits      = concatMap show nums
    stats       = foldr (\digit statsAcc -> Map.insertWith (+) digit 1 statsAcc) Map.empty digits
    (target, _) = maximumBy (comparing snd) (Map.toList stats)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = uniqInner lst []
  where
    uniqInner [] u     = u
    uniqInner (h:l) [] = uniqInner l [h]
    uniqInner (h:l) us = if null (filter (== h) us) 
      then
        uniqInner l (h : us)
      else
        uniqInner l us

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = grouped
  where
    pairs             = zip l (map f l)
    uniqResults       = uniq $ map snd pairs
    origsOfResult res = map fst $ filter (\pair -> res == snd pair) pairs
    grouped           = map (\r -> (r, origsOfResult r)) uniqResults