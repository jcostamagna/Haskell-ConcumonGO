module Main where

import Control.Concurrent (threadDelay)
import Game(game)

main :: IO ()
main = do
    putStrLn("Inicia el juego")
    game
    sleepMs 30
    putStrLn("Termina el juego")


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)
