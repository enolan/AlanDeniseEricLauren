module Main(main) where

import qualified Data.Map.Strict as M
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.Process
    (CreateProcess(..), readCreateProcessWithExitCode, shell)

import ADEL

main :: IO ()
main = do [envGood, envBad, cmd] <- getArgs
          envGood' <- convDumpvars <$> readFile envGood
          envBad'  <- convDumpvars <$> readFile envBad
          let diff = setDifference envGood' envBad'
          res <- minimalSubMapSatisfyingM diff (testFun envGood' cmd)
          print res

convDumpvars :: String -> M.Map String String
convDumpvars = M.fromList . read

testFun ::
    M.Map String String ->
    String ->
    M.Map String (SetDifference String) ->
    IO Bool
testFun original cmd diff = do
    let testEnv = applyDifference original diff
    isFailure <$> runProcessInEnv (shell cmd) testEnv

isFailure :: ExitCode -> Bool
isFailure ExitSuccess = False
isFailure _           = True

runProcessInEnv :: CreateProcess -> M.Map String String -> IO ExitCode
runProcessInEnv process vars = do
    let process' = process
            {cwd = Just (vars M.! "PWD"), env = Just $ M.toList vars}
    (exit, _, _) <- readCreateProcessWithExitCode process' ""
    return exit

-- Regular M.difference only gives you things that are in the second map but
-- not the first. We want to track things that are not in the second map but
-- are in the first, as well as keys that are in both maps with different
-- valuse.
data SetDifference v = Removed
                     | Added   v
                     | Changed v
                     deriving (Eq, Show, Read)

setDifference ::
    (Ord k, Eq v) => M.Map k v -> M.Map k v -> M.Map k (SetDifference v)
setDifference = M.mergeWithKey both left right where
    both _ av bv = if av == bv then Nothing else Just $ Changed bv
    left = M.map (const Removed)
    right = M.map Added

applyDifference :: Ord k => M.Map k v -> M.Map k (SetDifference v) -> M.Map k v
applyDifference = M.mergeWithKey both left right where
    both _ _ Removed     = Nothing
    both _ _ (Added _  ) = error "impossible, applyDifference both Added"
    both _ _ (Changed v) = Just v
    left = id
    right = M.map checkDiff
    checkDiff Removed     = error "impossible, applyDifference right Removed"
    checkDiff (Added   v) = v
    checkDiff (Changed _) = error "impossible, applyDifference right Changed"