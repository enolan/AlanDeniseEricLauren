{-# LANGUAGE BangPatterns #-}
-- | Construct minimal subsets and submaps satisfying arbitrary properties.
--   Based on the ADEL algorithm from "An Optimal Iterative Algorithm for
--   Extracting MUCs in a Black-box Constraint Network" Philippe Laborie, ECAI
--   2014. doi:10.3233/978-1-61499-419-0-1051 available at:
--   http://ebooks.iospress.nl/publication/37115

module ADEL (minimalSubMapSatisfyingM) where

import Control.Monad.Random
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import System.Random.Shuffle (shuffleM)


-- Given a map M and a function that describes an upward-closed property on
-- maps, return the minimal submap of M satisfying the property.
-- Assumptions:
--  - M satisfies the property.
--  - the property is "upward-closed" i.e. for all maps N where the property is
--    true, the property is true of all supermaps of N that are submaps of M.
minimalSubMapSatisfyingM :: forall m k v.
    (MonadRandom m, Ord k) => M.Map k v -> (M.Map k v -> m Bool) -> m (M.Map k v)
minimalSubMapSatisfyingM bigMap p = do
    shuffledMap <- V.fromList <$> (shuffleM $ M.toList bigMap)
    go M.empty shuffledMap 0 (V.length shuffledMap) 0 0
    where
    go :: M.Map k v -> V.Vector (k, v) -> Int -> Int -> Int -> Int ->
          m (M.Map k v)
    go candidate shuffledMap idx s elCount elDistanceSum = do
        newI <- findNext candidate shuffledMap idx p s
        case newI of
            Nothing    -> return candidate
            Just newI' ->
                let newCandidate =
                        M.union
                            candidate
                            (uncurry M.singleton $ shuffledMap V.! newI')
                    smallSubsetSoFar =
                        fromIntegral newI' <=
                        (logBase 2 $ fromIntegral $ M.size bigMap)
                    elDistanceSum' = elDistanceSum + newI' - idx
                    elCount' = elCount + 1
                    s' = elDistanceSum' `div` elCount'                        in
                    if smallSubsetSoFar
                        then do
                            newCandidateSatisifes <- p newCandidate
                            if newCandidateSatisifes
                                then return newCandidate
                                else go
                                    newCandidate
                                    shuffledMap
                                    newI'
                                    s'
                                    elCount'
                                    elDistanceSum'
                        else go
                                newCandidate
                                shuffledMap
                                newI'
                                s'
                                elCount'
                                elDistanceSum'

-- Finds the largest index j such that P is true of the candidate unioned with
-- the members of the map to be minimized with indices >= j. Constraint: j >= i.
-- If P is true of the candidate unmodified, returns Nothing.
findNext :: (Ord k, Monad m) =>
    M.Map k v -> V.Vector (k, v) -> Int -> (M.Map k v -> m Bool) -> Int ->
    m (Maybe Int)
findNext candidate shuffledMap idx p s = do
    mbInterval <- accelerate candidate shuffledMap idx s p
    case mbInterval of
        Nothing -> return Nothing
        Just (lower, upper) ->
            Just <$> dichotomize candidate shuffledMap lower upper p

-- Acceleration step. We either find P is true of the candidate under
-- consideration, or we find an index l such that P is not true of the
-- candidate under consideration unioned with the elements of index >= than l.
accelerate :: (Ord k, Monad m) =>
    M.Map k v -> V.Vector (k, v) -> Int -> Int -> (M.Map k v -> m Bool) ->
    m (Maybe (Int, Int))
accelerate candidate shuffledMap idx s p =
    accelerate' candidate shuffledMap idx s p idx

accelerate' :: (Ord k, Monad m) =>
    M.Map k v -> V.Vector (k, v) -> Int -> Int -> (M.Map k v -> m Bool) -> Int ->
    m (Maybe (Int, Int))
accelerate' candidate shuffledMap idx s p lower = do
    let len = V.length shuffledMap
        -- First index we're unioning.
        l = min len (idx + s)
        -- U_l->
        elsToAdd = sliceToEnd l shuffledMap
        -- X union U_l->
        newCandidate = M.union candidate (vecToMap elsToAdd)
    newCandidateSatisifes <- p newCandidate
    if newCandidateSatisifes
        then
            if l == len
                -- P is true of the candidate, because elsToAdd = []
                then return Nothing
                -- Otherwise, continue testing exponentially smaller submaps.
                --
                -- Can't easily preserve newCandidate here, because it shrinks
                -- rather than grows each iteration. I'm not sure if this
                -- affects the complexity analysis.
                else accelerate' candidate shuffledMap idx (s*2) p l
        else
            -- P was true of candidate union U_idx+(s/2)->, which we examined
            -- in the last iteration, unless this is the first iteration.
            return $ Just (lower, l - 1)

-- dichotomization takes the result of acceleration and finds the exact answer
-- by binary search. the result of acceleration is the upper bound, the lower
-- bound is idx+s/2
dichotomize :: (Ord k, Monad m) =>
    M.Map k v -> V.Vector (k, v) -> Int -> Int -> (M.Map k v -> m Bool) -> m Int
dichotomize candidate shuffledMap lower upper p = do
    let midpoint = ceiling $ (fromIntegral lower + fromIntegral upper) / 2
        -- U_m->
        elsToAdd = sliceToEnd midpoint shuffledMap
        -- X union U_m->
        newCandidate = M.union candidate (vecToMap elsToAdd)
    newCandidateSatisifes <- p newCandidate
    if lower /= upper
        then if newCandidateSatisifes
            then dichotomize candidate shuffledMap midpoint  upper         p
            else dichotomize candidate shuffledMap lower    (midpoint - 1) p
        else return lower

vecToMap :: Ord k => V.Vector (k, v) -> M.Map k v
vecToMap = V.foldl' (\m (k, v) -> M.insert k v m) M.empty

-- Slice from a given index (inclusive) to the end of a vector.
sliceToEnd :: Int -> V.Vector a -> V.Vector a
sliceToEnd x v = let len = V.length v in if x > len then
    error "sliceToEnd: index out of range" else
    V.slice x (len - x) v