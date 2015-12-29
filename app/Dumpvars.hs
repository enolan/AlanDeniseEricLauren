import System.Environment (getEnvironment)

main :: IO ()
main = getEnvironment >>= print
