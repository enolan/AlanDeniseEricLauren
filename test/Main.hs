{-# LANGUAGE FlexibleInstances,TupleSections,TypeFamilies #-}
import Control.Exception (ErrorCall(..), try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRand, Rand, RandT)
import Data.Functor.Identity (Identity)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Ord (Down(..))
import System.Random (getStdGen, mkStdGen, RandomGen, StdGen)
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ADEL

main :: IO ()
main = hspec $
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
