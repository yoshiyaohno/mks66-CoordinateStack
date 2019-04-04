import System.Environment
import Text.Printf

main = do
    args <- getArgs
    let count = read $ head args :: Int
        s = [(printf "apply\nsave\nframe%03d.png\n" i) | i <- [0..count]]
    putStrLn $ concat s
