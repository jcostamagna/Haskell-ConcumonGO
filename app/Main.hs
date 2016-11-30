module Main where

import Control.Concurrent (threadDelay, setNumCapabilities)
import Game(game)



main :: IO ()
main = do
    putStrLn("Inicia el juego")
    --setNumCapabilities 3
    game
    sleepMs 20
    putStrLn("TERMINA LA SIMULACION")


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)
