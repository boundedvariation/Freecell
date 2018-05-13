import FreeCell
import System.Environment

main = do
    fn <- getArgs
    solveFile (head fn)
