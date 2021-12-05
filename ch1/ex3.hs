module Main where

main :: IO ()
main = do 
    putStrLn ("What's your name?")
    name <- getLine 
    putStrLn ("Hi " ++ name)