module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let (arg1,arg2) = (read $ args !! 0,read $ args !! 1) :: (Integer,Integer)
    let result = arg1 + arg2
    putStrLn ("Result = " ++ show result)
