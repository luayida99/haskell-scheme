module Main where 

import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    putStrLn ("Hi, " ++ args!!0 ++ ", " ++ args!!1)