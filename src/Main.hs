module Main where

import Data.Rebin
import System.Environment

-- | Reads three filenames from the arguments and outputs to STDOUT
main :: IO ()
main = do
    args <- getArgs
    case args of
        [x1fp,y1fp,x2fp] -> do
            x1 <- loadData x1fp
            y1 <- loadData y1fp
            x2 <- loadData x2fp
            let y2 = rebin x1 y1 x2
            mapM_ print y2
        _ -> usage
    return ()

-- | Prints example usage
usage :: IO ()
usage = do
    putStrLn "Example Usage: ./hs-rebin x1.txt y1.txt x2.txt"

-- | Load a list of numbers from a file
loadData :: FilePath -> IO [Double]
loadData fp = do
    contents <- readFile fp
    return $ (map read . words) contents