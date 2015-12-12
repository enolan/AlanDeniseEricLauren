module Main where

import Control.Exception (bracket)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.Process
    (CreateProcess(..), createProcess, proc, waitForProcess)

main :: IO (M.Map String (SetDifference String))
main = do
    let candidateEnvs = map (applyDifference goodEnvMyMSYS) candidatesDiffs
    results <- mapM (\e -> test e >>= \r -> return (e,r)) candidateEnvs
    let failures = filter (isFailure . snd) results
    let failureDiffs = map (setDifference goodEnvMyMSYS . fst) failures
    let bestFailure = head $ sortBy (compare `on` M.size) failureDiffs
    return bestFailure

isFailure :: ExitCode -> Bool
isFailure ExitSuccess = False
isFailure _           = True

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy _ [] = []
wordsBy p xs = case break p xs of
                    (f, xs') -> case xs' of
                        [] -> [f]
                        _:xs'' -> f : wordsBy p xs''

test :: M.Map String String -> IO ExitCode
test env = runMakeInEnv env

clearEnv :: IO ()
clearEnv = do vars <- getEnvironment
              mapM_ (unsetEnv . fst) vars

setEnvironment :: [(String, String)] -> IO ()
setEnvironment vars = do clearEnv
                         mapM_ (uncurry setEnv) vars

runProcessInEnv :: CreateProcess -> M.Map String String -> IO ExitCode
runProcessInEnv process vars =
    bracket (getEnvironment <* setEnvironment (M.toList vars))
             setEnvironment
            (const $ do
                (_, _, _, h) <- createProcess
                    (process {cwd = M.lookup "PWD" vars})
                waitForProcess h)

runMakeInEnv :: M.Map String String -> IO ExitCode
runMakeInEnv = runProcessInEnv (proc "make" ["foo"])

goodEnvMyMSYS :: M.Map String String
goodEnvMyMSYS = M.fromList [("!;",";\\"),("ALLUSERSPROFILE","C:\\ProgramData"),("ANSICON","240x1000 (240x57)"),("ANSICON_DEF","7"),("APPDATA","C:\\Users\\Echo\\AppData\\Roaming"),("COMMONPROGRAMFILES","C:\\Program Files\\Common Files"),("COMMONPROGRAMFILES(X86)","C:\\Program Files (x86)\\Common Files"),("COMMONPROGRAMW6432","C:\\Program Files\\Common Files"),("COMPUTERNAME","BEHEMOTH-W7"),("COMSPEC","C:\\Windows\\system32\\cmd.exe"),("CONEMUANSI","ON"),("CONEMUBACKHWND","0x0003037C"),("CONEMUBASEDIR","C:\\Program Files\\ConEmu\\ConEmu"),("CONEMUBUILD","151208"),("CONEMUDIR","C:\\Program Files\\ConEmu"),("CONEMUDRAWHWND","0x0003037E"),("CONEMUDRIVE","C:"),("CONEMUHOOKS","Enabled"),("CONEMUHWND","0x00120272"),("CONEMUPID","656"),("CONEMUSERVERPID","2348"),("CONEMUWORKDIR","C:\\Users\\Echo"),("CONEMUWORKDRIVE","C:"),("FP_NO_HOST_CHECK","NO"),("HOME","C:\\msys64\\home\\Echo"),("HOMEDRIVE","C:"),("HOMEPATH","\\Users\\Echo"),("HOSTNAME","Behemoth-W7"),("INFOPATH","C:\\msys64\\usr\\local\\info;C:\\msys64\\usr\\share\\info;C:\\msys64\\usr\\info;C:\\msys64\\share\\info"),("JD2_HOME","C:\\Users\\Echo\\AppData\\Local\\JDownloader v2.0"),("LANG","en_US.UTF-8"),("LOCALAPPDATA","C:\\Users\\Echo\\AppData\\Local"),("LOGONSERVER","\\\\BEHEMOTH-W7"),("MANPATH","C:\\msys64\\usr\\local\\man;C:\\msys64\\usr\\share\\man;C:\\msys64\\usr\\man;C:\\msys64\\share\\man"),("MSYSTEM","MSYS"),("NUMBER_OF_PROCESSORS","8"),("OLDPWD","C:/msys64/home/Echo/reducecase"),("OS","Windows_NT"),("PATH","C:\\msys64\\usr\\local\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\opt\\bin;C:\\Program Files\\ConEmu;C:\\Program Files\\ConEmu\\ConEmu;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files (x86)\\Windows Kits\\8.1\\Windows Performance Toolkit;C:\\Program Files\\Microsoft SQL Server\\110\\Tools\\Binn;C:\\Program Files (x86)\\Microsoft SDKs\\TypeScript\\1.0;C:\\Program Files (x86)\\Skype\\Phone;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Users\\Echo\\AppData\\Roaming\\local\\bin;C:\\msys64\\usr\\bin\\site_perl;C:\\msys64\\usr\\bin\\vendor_perl;C:\\msys64\\usr\\bin\\core_perl"),("PATHEXT",".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC"),("PKG_CONFIG_PATH","C:\\msys64\\usr\\lib\\pkgconfig;C:\\msys64\\usr\\share\\pkgconfig;C:\\msys64\\lib\\pkgconfig"),("PRINTER","Microsoft XPS Document Writer"),("PROCESSOR_ARCHITECTURE","AMD64"),("PROCESSOR_IDENTIFIER","Intel64 Family 6 Model 42 Stepping 7, GenuineIntel"),("PROCESSOR_LEVEL","6"),("PROCESSOR_REVISION","2a07"),("PROGRAMDATA","C:\\ProgramData"),("PROGRAMFILES","C:\\Program Files"),("PROGRAMFILES(X86)","C:\\Program Files (x86)"),("PROGRAMW6432","C:\\Program Files"),("PS1","\\[\\e]0;\\w\\a\\]\\n\\[\\e[32m\\]\\u@\\h \\[\\e[35m\\]$MSYSTEM\\[\\e[0m\\] \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$ "),("PSMODULEPATH","C:\\Windows\\system32\\WindowsPowerShell\\v1.0\\Modules\\"),("PUBLIC","C:\\Users\\Public"),("PWD","C:/msys64/home/Echo/simple"),("SESSIONNAME","Console"),("SHELL","C:/msys64/usr/bin/bash"),("SHLVL","1"),("SYSTEMDRIVE","C:"),("SYSTEMROOT","C:\\Windows"),("TEMP","C:\\Users\\Echo\\AppData\\Local\\Temp"),("TERM","xterm-256color"),("TMP","C:\\Users\\Echo\\AppData\\Local\\Temp"),("USER","Echo"),("USERDOMAIN","Behemoth-W7"),("USERNAME","Echo"),("USERPROFILE","C:\\Users\\Echo"),("VS110COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\Common7\\Tools\\"),("VS120COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\Tools\\"),("VS140COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Common7\\Tools\\"),("WINDIR","C:\\Windows"),("WINDOWS_TRACING_FLAGS","3"),("WINDOWS_TRACING_LOGFILE","C:\\BVTBin\\Tests\\installpackage\\csilogfile.log"),("_","C:/Users/Echo/AppData/Roaming/local/bin/stack")]

badEnvMyMSYS :: M.Map String String
badEnvMyMSYS = M.fromList [("!;",";\\"),("ALLUSERSPROFILE","C:\\ProgramData"),("ANSICON","240x1000 (240x57)"),("ANSICON_DEF","7"),("APPDATA","C:\\Users\\Echo\\AppData\\Roaming"),("COMMONPROGRAMFILES","C:\\Program Files\\Common Files"),("COMMONPROGRAMFILES(X86)","C:\\Program Files (x86)\\Common Files"),("COMMONPROGRAMW6432","C:\\Program Files\\Common Files"),("COMPUTERNAME","BEHEMOTH-W7"),("COMSPEC","C:\\Windows\\system32\\cmd.exe"),("CONEMUANSI","ON"),("CONEMUBACKHWND","0x0003037C"),("CONEMUBASEDIR","C:\\Program Files\\ConEmu\\ConEmu"),("CONEMUBUILD","151208"),("CONEMUDIR","C:\\Program Files\\ConEmu"),("CONEMUDRAWHWND","0x0003037E"),("CONEMUDRIVE","C:"),("CONEMUHOOKS","Enabled"),("CONEMUHWND","0x00120272"),("CONEMUPID","656"),("CONEMUSERVERPID","2348"),("CONEMUWORKDIR","C:\\Users\\Echo"),("CONEMUWORKDRIVE","C:"),("FP_NO_HOST_CHECK","NO"),("GHC_PACKAGE_PATH","C:\\msys64\\home\\Echo\\simple\\.stack-work\\install\\3f481b39\\pkgdb;C:\\Users\\Echo\\AppData\\Roaming\\stack\\snapshots\\0aef8980\\pkgdb;C:\\Users\\Echo\\AppData\\Local\\Programs\\stack\\x86_64-windows\\ghc-7.10.2\\lib\\package.conf.d"),("HASKELL_DIST_DIR",".stack-work\\dist\\d96ab9d9"),("HASKELL_PACKAGE_SANDBOX","C:\\Users\\Echo\\AppData\\Roaming\\stack\\snapshots\\0aef8980\\pkgdb"),("HASKELL_PACKAGE_SANDBOXES","C:\\msys64\\home\\Echo\\simple\\.stack-work\\install\\3f481b39\\pkgdb;C:\\Users\\Echo\\AppData\\Roaming\\stack\\snapshots\\0aef8980\\pkgdb;"),("HOME","C:\\msys64\\home\\Echo"),("HOMEDRIVE","C:"),("HOMEPATH","\\Users\\Echo"),("HOSTNAME","Behemoth-W7"),("INFOPATH","C:\\msys64\\usr\\local\\info;C:\\msys64\\usr\\share\\info;C:\\msys64\\usr\\info;C:\\msys64\\share\\info"),("JD2_HOME","C:\\Users\\Echo\\AppData\\Local\\JDownloader v2.0"),("LANG","en_US.UTF-8"),("LOCALAPPDATA","C:\\Users\\Echo\\AppData\\Local"),("LOGONSERVER","\\\\BEHEMOTH-W7"),("MANPATH","C:\\msys64\\usr\\local\\man;C:\\msys64\\usr\\share\\man;C:\\msys64\\usr\\man;C:\\msys64\\share\\man"),("MSYSTEM","MSYS"),("NUMBER_OF_PROCESSORS","8"),("OLDPWD","C:/msys64/home/Echo/reducecase"),("OS","Windows_NT"),("PATH","C:\\msys64\\home\\Echo\\simple\\.stack-work\\install\\3f481b39\\bin;C:\\Users\\Echo\\AppData\\Roaming\\stack\\snapshots\\0aef8980\\bin;C:\\Users\\Echo\\AppData\\Local\\Programs\\stack\\x86_64-windows\\ghc-7.10.2\\bin;C:\\Users\\Echo\\AppData\\Local\\Programs\\stack\\x86_64-windows\\ghc-7.10.2\\mingw\\bin;C:\\Users\\Echo\\AppData\\Local\\Programs\\stack\\x86_64-windows\\msys2-20150512\\usr\\bin;C:\\msys64\\usr\\local\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\usr\\bin;C:\\msys64\\opt\\bin;C:\\Program Files\\ConEmu;C:\\Program Files\\ConEmu\\ConEmu;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files (x86)\\Windows Kits\\8.1\\Windows Performance Toolkit;C:\\Program Files\\Microsoft SQL Server\\110\\Tools\\Binn;C:\\Program Files (x86)\\Microsoft SDKs\\TypeScript\\1.0;C:\\Program Files (x86)\\Skype\\Phone;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Users\\Echo\\AppData\\Roaming\\local\\bin;C:\\msys64\\usr\\bin\\site_perl;C:\\msys64\\usr\\bin\\vendor_perl;C:\\msys64\\usr\\bin\\core_perl"),("PATHEXT",".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC"),("PKG_CONFIG_PATH","C:\\msys64\\usr\\lib\\pkgconfig;C:\\msys64\\usr\\share\\pkgconfig;C:\\msys64\\lib\\pkgconfig"),("PRINTER","Microsoft XPS Document Writer"),("PROCESSOR_ARCHITECTURE","AMD64"),("PROCESSOR_IDENTIFIER","Intel64 Family 6 Model 42 Stepping 7, GenuineIntel"),("PROCESSOR_LEVEL","6"),("PROCESSOR_REVISION","2a07"),("PROGRAMDATA","C:\\ProgramData"),("PROGRAMFILES","C:\\Program Files"),("PROGRAMFILES(X86)","C:\\Program Files (x86)"),("PROGRAMW6432","C:\\Program Files"),("PS1","\\[\\e]0;\\w\\a\\]\\n\\[\\e[32m\\]\\u@\\h \\[\\e[35m\\]$MSYSTEM\\[\\e[0m\\] \\[\\e[33m\\]\\w\\[\\e[0m\\]\\n\\$ "),("PSMODULEPATH","C:\\Windows\\system32\\WindowsPowerShell\\v1.0\\Modules\\"),("PUBLIC","C:\\Users\\Public"),("PWD","C:/msys64/home/Echo/simple"),("SESSIONNAME","Console"),("SHELL","C:/msys64/usr/bin/bash"),("SHLVL","1"),("STACK_EXE","C:\\Users\\Echo\\AppData\\Roaming\\local\\bin\\stack.exe"),("SYSTEMDRIVE","C:"),("SYSTEMROOT","C:\\Windows"),("TEMP","C:\\Users\\Echo\\AppData\\Local\\Temp"),("TERM","xterm-256color"),("TMP","C:\\Users\\Echo\\AppData\\Local\\Temp"),("USER","Echo"),("USERDOMAIN","Behemoth-W7"),("USERNAME","Echo"),("USERPROFILE","C:\\Users\\Echo"),("VS110COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 11.0\\Common7\\Tools\\"),("VS120COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\Tools\\"),("VS140COMNTOOLS","C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\Common7\\Tools\\"),("WINDIR","C:\\Windows"),("WINDOWS_TRACING_FLAGS","3"),("WINDOWS_TRACING_LOGFILE","C:\\BVTBin\\Tests\\installpackage\\csilogfile.log"),("_","C:/Users/Echo/AppData/Roaming/local/bin/stack")]

goodBadDiff :: M.Map String (SetDifference String)
goodBadDiff = setDifference goodEnvMyMSYS badEnvMyMSYS

candidatesDiffs :: [M.Map String (SetDifference String)]
candidatesDiffs = allSubmaps goodBadDiff

allSubmaps :: Ord k => M.Map k v -> [M.Map k v]
allSubmaps = go . M.toList where
    go :: Ord k => [(k, v)] -> [M.Map k v]
    go []             = [M.empty]
    go ((k, v): rest) = map (M.insert k v) (go rest) ++ go rest

-- Regular M.difference only gives you things that are in the second map but
-- not the first. We want to track things that are not in the second map but
-- are in the first, as well as keys that are in both maps with different
-- valuse.
data SetDifference v = Removed
                     | Added   v
                     | Changed v
                     deriving (Show, Read)

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