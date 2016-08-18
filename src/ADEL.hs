-- | Construct minimal submaps satisfying arbitrary properties. This extends to
--   sets by simply creating a @Map k ()@. Based on the ADEL algorithm from "An
--   Optimal Iterative Algorithm for Extracting MUCs in a Black-box Constraint
--   Network" Philippe Laborie, ECAI 2014. doi:10.3233/978-1-61499-419-0-1051
--   available at: http://ebooks.iospress.nl/publication/37115

module ADEL
  (minimalSubmapSatisfying
  , minimalDifferenceSatisfying
  , KeyDiff(..)
  , mapDifference
  , applyDifference) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import System.Random.Shuffle (shuffleM)

-- | Given a map M and a function that describes an upward-closed property on
--   maps, return the minimal submap of M satisfying the property.
--
--   Assumptions:
--
--     * M satisfies the property.
--
--     * the property is "upward-closed" i.e. for all maps N where the property
--       is true, the property is true of all supermaps of N.
--
--   This is a Las Vegas-ish algorithm. You need 'MonadRandom' but the result is
--   deterministic provided the passed predicate is deterministic and there is a
--   unique minimal submap.
minimalSubmapSatisfying :: (MonadRandom m, Ord k) =>
  M.Map k v -> (M.Map k v -> m Bool) -> m (M.Map k v)
minimalSubmapSatisfying bigMap p = do
  trueOfWholeMap <- p bigMap
  unless trueOfWholeMap $ error
    "minimalSubMapSatisfyingM: supplied property isn't true of supplied map"
  runADELT bigMap p $ go 0 (M.size bigMap) 0 0
  where
    go idx step elDistanceSum elCount = do
      shuffled <- fst <$> ask
      newI <- findNext idx step
      case newI of
        Nothing -> get
        Just newI' -> do
          let smallSubsetSoFar = (fromIntegral newI' :: Double) <=
                logBase 2 (fromIntegral $ M.size bigMap)
              elDistanceSum' = elDistanceSum + newI' + 1 - idx
              elCount' = elCount + 1
              step' = elDistanceSum' `div` elCount'
              recur = go (newI' + 1) step' elDistanceSum' elCount'
          if smallSubsetSoFar
            then do
              modify $ \m -> V.foldl (\m' (k, _) -> M.delete k m') m (sliceToEnd (newI' + 1) shuffled)
              wereDone <- mapSatisfies
              if wereDone
                then get
                else do addSlice (newI' + 1) (V.length shuffled - newI' - 1)
                        recur
            else recur

-- We need to carry around: the map being minimized, a random permutation of the
-- keys, and the property of interest.
type ADELT k v m a = (Ord k, Monad m) =>
  StateT (M.Map k v) (ReaderT (V.Vector (k, v), M.Map k v -> m Bool) m) a

runADELT :: (Ord k, MonadRandom m) =>
  M.Map k v -> (M.Map k v -> m Bool) -> ADELT k v m a -> m a
runADELT m p adelT = do
  shuffledKeys <- V.fromList <$> shuffleM (M.toList m)
  runReaderT (evalStateT adelT m) (shuffledKeys, p)

mapSatisfies :: ADELT k v m Bool
mapSatisfies = do
  p <- snd <$> ask
  mapBeingConstructed <- get
  -- Never change, monad transformers
  lift $ lift $ p mapBeingConstructed

-- Find the next element of the minimal submap or return Nothing if the current
-- candidate already satisfies the property.
-- Postcondition: the map has exactly the elements between idx and the return
-- value removed.
findNext :: Int -> Int -> ADELT k v m (Maybe Int)
findNext idx step = do
  mbInterval <- accelerate idx step
  case mbInterval of
    Nothing             -> return Nothing
    Just (lower, upper) ->
      Just <$> dichotomize lower upper

-- Acceleration step: we test exponentially smaller submaps until we either find
-- the interval the next element of the minimal submap must be in, or find out
-- the property is true with all the elements after idx removed.
-- Precondition: the property is satisfied
-- Postcondition:
--   - if Just is returned, the map has exactly the elements with indices
--     between idx and the returned upper bound removed and the property is not
--     satisfied.
--   - if Nothing is returned, the map has all elements after idx removed.
accelerate :: Int -> Int -> ADELT k v m (Maybe (Int, Int))
accelerate idx step = do
  shuffled <- fst <$> ask
  if idx >= V.length shuffled
    then return Nothing
    else do modify $ \m -> rmSlice idx step m shuffled
            accelerate' idx step idx

-- Precondition: the elements between idx and idx + step and not in the map.
accelerate' :: Int -> Int -> Int -> ADELT k v m (Maybe (Int, Int))
accelerate' idx step lowerBound = do
  shuffled <- fst <$> ask
  satisfies <- mapSatisfies
  if satisfies
    then if idx + step < V.length shuffled
            then do modify $ \m -> rmSlice (idx + step) step m shuffled
                    accelerate' idx (step * 2) (idx + step)
            else return Nothing
    else return $ Just (lowerBound, min (V.length shuffled) (idx + step) - 1)

-- Remove the len elements in a vector starting at start from a map.
rmSlice :: Ord k => Int -> Int -> M.Map k v -> V.Vector (k, v) -> M.Map k v
rmSlice start len inMap elsVec =
  V.foldl' (\m (k,_) -> M.delete k m) inMap elsToRemove
  where elsToRemove = if start + len <= V.length elsVec
          then V.slice start len elsVec
          else sliceToEnd start elsVec

addSlice :: Int -> Int -> ADELT k v m ()
addSlice idx len = do
  vect <- fst <$> ask
  let sliceToAdd = V.slice idx len vect
  modify $ \m -> V.foldl
    (\m' (k, v) -> M.insert k v m')
    m
    sliceToAdd

-- Slice from a given index (inclusive) to the end of a vector.
sliceToEnd :: Int -> V.Vector a -> V.Vector a
sliceToEnd x v = let len = V.length v in if x > len then
    error "sliceToEnd: index out of range" else
    V.slice x (len - x) v

-- Given an interval, find the next element of the minimal submap by binary
-- search.
-- Precondition:
--   - the elements with indices greater than the upper bound are in the map
--   - the minimal next element has an index between the lower and upper bounds
-- Postcondition: All the elements after the initial lower bound and before the
-- returned index are removed.
dichotomize :: Int -> Int -> ADELT k v m Int
dichotomize lower upper = do
  let midpoint' = midpoint lower upper
  addSlice midpoint' (upper - midpoint' + 1)
  dichotomize' lower upper

-- Precondition: the elements with indices >= ceil((lower + upper) / 2) are in
-- the map.
dichotomize' :: Int -> Int -> ADELT k v m Int
dichotomize' lower upper = do
  shuffled <- fst <$> ask
  let midpoint' = midpoint lower upper
  if lower == upper
    then do addSlice lower 1
            return lower
    else do
      satisfies <- mapSatisfies
      if satisfies
        then do let newMidpoint = midpoint midpoint' upper
                modify $ \m -> rmSlice midpoint' (newMidpoint - midpoint') m shuffled
                dichotomize' midpoint' upper
        else do let newMidpoint = midpoint lower (midpoint' - 1)
                addSlice newMidpoint (midpoint' - newMidpoint)
                dichotomize' lower (midpoint' - 1)

midpoint :: Int -> Int -> Int
midpoint lower upper = ceiling $
        (fromIntegral lower + fromIntegral upper :: Double) / 2

-- | A change in a key of a 'M.Map'.
data KeyDiff v = Removed
               | Added   v
               | Changed v
  deriving (Eq, Show, Read)

-- | An invertible map difference. Regular 'M.difference' only gives you things
-- that are in the first map but not the second. This tracks keys that are
-- in the second map but not in the first, as well as keys that are in both maps
-- with different values.
mapDifference ::
    (Ord k, Eq v) => M.Map k v -> M.Map k v -> M.Map k (KeyDiff v)
mapDifference = M.mergeWithKey both left right where
    both _ av bv = if av == bv then Nothing else Just $ Changed bv
    left = M.map (const Removed)
    right = M.map Added

-- | Given a map and and a difference, apply the difference.
--
-- prop> applyDifference m (mapDifference m n) = n
applyDifference :: Ord k => M.Map k v -> M.Map k (KeyDiff v) -> M.Map k v
applyDifference = M.mergeWithKey both left right where
    both _ _ Removed     = Nothing
    both _ _ (Added _  ) = error "impossible, applyDifference both Added"
    both _ _ (Changed v) = Just v
    left = id
    right = M.map checkDiff
    checkDiff Removed     = error "impossible, applyDifference right Removed"
    checkDiff (Added   v) = v
    checkDiff (Changed _) = error "impossible, applyDifference right Changed"

-- | Given a property P, a map for which P is false and a map for which P is
-- true, find the minimal set of changes from the first map to the second map
-- that makes the property true.
--
-- The comments re randomness from 'minimalSubmapSatisfying' applies.
minimalDifferenceSatisfying :: (Eq v, Ord k, MonadRandom m) =>
  M.Map k v -> M.Map k v -> (M.Map k v -> m Bool) -> m (M.Map k (KeyDiff v))
minimalDifferenceSatisfying falseSet trueSet prop =
  let largestDifference = mapDifference falseSet trueSet
      prop' diff = prop $ applyDifference falseSet diff in
  minimalSubmapSatisfying largestDifference prop'
