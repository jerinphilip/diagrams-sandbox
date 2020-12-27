{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Data.List (unfoldr, sort)


zeroElement = square 2 # lc black # fc red # lw 0.1
nonZeroElement = square 2 # lc black # fc green # lw 0.1

paddedSequence maxLength length = 
        hsep 1 $ (++)
                    (replicate length nonZeroElement )
                    (replicate (maxLength - length) zeroElement)

paddedSequences maxLength sequenceLengths = 
        vsep 1 $ map (paddedSequence maxLength) sequenceLengths

demo :: Int -> [Int] -> Diagram B
demo maxLength sequenceLengths = pad 2 $ paddedSequences maxLength sequenceLengths

-- https://stackoverflow.com/q/30740366/4565794
randomList :: Int -> Int -> Int -> IO [Int]
randomList n lb ub = sequence $ replicate n $ randomRIO (lb, ub)

main = do 
    let maxLength = 16
    let batchSize = 20
    sequenceLengths <- randomList batchSize 1 maxLength
    mainWith $ demo maxLength $ sort sequenceLengths
