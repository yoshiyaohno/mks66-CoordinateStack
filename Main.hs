import Line
import Transform
import Solids
import Parse
import DrawMats

import Control.Monad.State
import System.IO
import System.Environment
--import System.Process
--import System.Directory

main = do
    args <- getArgs
    script <- readFile (head args)
    let cmds = parse $ lines script :: [StateT DrawMats IO ()]
    runStateT (sequence_ cmds) emptyDM
