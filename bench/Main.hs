{-# LANGUAGE TupleSections #-}
module Main (main) where

import Criterion.Main
import qualified Data.Map.Strict as M

import ADEL

mapUpTo :: Int -> M.Map Int ()
mapUpTo x = M.fromList $ map (,()) [0 .. x]

findNofM :: Int -> Int -> Benchmark
findNofM numToFind totalNum = if numToFind > totalNum
  then error "findNofM numToFind > totalNum"
  else env
         (return (mapUpTo numToFind, mapUpTo totalNum))
         (\ ~(find, whole) -> bench
           ("find " ++ show numToFind ++ " of " ++ show totalNum)
           (nfIO $ minimalSubmapSatisfying whole (return . M.isSubmapOf find)))

findHalf :: Int -> Benchmark
findHalf n = findNofM (n `div` 2) n

findNone :: Int -> Benchmark
findNone = findNofM 0

findAll :: Int -> Benchmark
findAll n = findNofM n n

main = defaultMain $ concat
  [map findHalf [0,10..200],
   map findNone [0,10..200],
   map findAll  [0,10..200]]
