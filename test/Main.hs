{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections,TypeFamilies #-}
import Control.Exception (ErrorCall(..), try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRand, Rand, RandT)
import Data.Functor.Identity (Identity)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import Data.Ord (Down(..))
import System.Random (getStdGen, mkStdGen, RandomGen, StdGen)
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ADEL

main :: IO ()
main = hspec $ do
    describe "minimalSubmapSatisfying" $ do
        before getStdGen $ do
            it "finds the minimal submap of empty maps" $ idRand $ do
                res <- minimalSubmapSatisfying
                    (makeSizedMap 0)
                    (const (return True))
                return (res `shouldBe` M.empty)
            it "finds a subset of size 5" $ idRand $ do
                res <- minimalSubmapSatisfying
                    (makeSizedMap 200)
                    (\m -> return $ M.size m >= 5)
                return (res `shouldSatisfy` (\m -> M.size m == 5))
        prop "finds a subset of arbitrary size" $
            \sizeA sizeB -> idRand $ do
                let [NonNegative smaller, NonNegative bigger] =
                        sort [sizeA, sizeB]
                submap <- minimalSubmapSatisfying
                    (makeSizedMap bigger)
                    (\m -> return $ M.size m >= smaller)
                return $ M.size submap == smaller
        prop "finds a specific arbitrary subset" $
            \nums -> not (null nums) ==> idRand $ do
                let nums' :: [Int] = map (\(Down n) -> n) $ sort $
                        map (Down . getPositive) nums
                    bigSet = makeSizedMap (head nums')
                    subsetNums = M.fromList $ map (,()) $ tail nums'
                res <- minimalSubmapSatisfying
                    bigSet
                    (return . M.isSubmapOf subsetNums)
                return $ res == subsetNums
        prop
            "throws an exception if the property is not true of the argument"
            $ \size -> ioProperty $ do
                res <- try (minimalSubmapSatisfying
                           (makeSizedMap size)
                           (return . const False))
                case res of
                    Left (ErrorCall _) -> return True
                    _                  -> return False
    describe "minimalDifferenceSatisfying" $ do
        prop "finds minimal additions to an empty set"
          $ \(els :: [(Int, ())]) -> idRand $ do
            let targetSet = M.fromList els
            res <- minimalDifferenceSatisfying
              M.empty
              targetSet
              (\s -> return $ targetSet `M.isSubmapOf` s)
            return $ res == fmap Added targetSet
        prop "adds the minimal set with arbitrary false and true parameters"
          $ \(elsTrue :: [(Int, ())]) cutoff (elsFalse :: [(Int, ())]) ->
            cutoff < length elsTrue &&
            disjoint (M.fromList elsTrue) (M.fromList elsFalse) ==>
            idRand $ do
              let alreadyThere = M.fromList $ take cutoff elsTrue
                  falseSet = M.fromList elsFalse
                  trueSet = M.fromList elsTrue
                  expectedResult = fmap Added (M.difference trueSet alreadyThere)
              res <- minimalDifferenceSatisfying
                (falseSet `M.union` alreadyThere)
                (falseSet `M.union` trueSet)
                (\s -> return $ trueSet `M.isSubmapOf` s)
              return $ res === expectedResult
        prop "removes the correct arbitrary element" $
          \(els1 :: [(Int, Char)])
           (els2 :: [(Int, Char)])
           (toRemove :: (Int, Char)) -> idRand $ do
            let trueSet = M.delete (fst toRemove) (M.fromList els1)
                falseSet = uncurry M.insert toRemove $ M.fromList els2
            res <- minimalDifferenceSatisfying
              falseSet
              trueSet
              (return . isNothing . M.lookup (fst toRemove))
            return (res === M.singleton (fst toRemove) Removed)

disjoint :: (Ord k, Eq v) => M.Map k v -> M.Map k v -> Bool
disjoint m1 m2 = M.intersection m1 m2 == M.empty

instance RandomGen g => Example (RandT g Identity Expectation) where
    type Arg (RandT g Identity Expectation) = g
    evaluateExample randEx params act progressCallback = do
        resRef <- newIORef Nothing
        act $ \gen -> writeIORef resRef $ Just $ evalRand randEx gen
        res <- fromJust <$> readIORef resRef
        evaluateExample res params (\act' -> act' ()) progressCallback

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

idRand :: Rand StdGen a -> Rand StdGen a
idRand = id

makeSizedMap :: Int -> M.Map Int ()
makeSizedMap 0 = M.empty
makeSizedMap n | n > 0 = M.insert n () $ makeSizedMap (n-1)
               | otherwise = error "makeSizedMap negative"

instance Testable prop => Testable (Rand StdGen prop) where
    property randAct = property $ \g -> evalRand randAct g
