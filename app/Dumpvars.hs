import System.Environment (getEnvironment)

main = getEnvironment >>= print
