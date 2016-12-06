module Main where

import Control.Concurrent (threadDelay, setNumCapabilities, forkIO, killThread)
import Game(game)
import System.IO



main :: IO ()
main = do
    putStrLn("Inicia el juego")
    --setNumCapabilities 3
    threadChildId <- forkIO game
    c <- getChar
    killThread threadChildId
    putStrLn("TERMINA LA SIMULACION")


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)
