module Player where

import Control.Concurrent (threadDelay)


playerPlay id = do
    putStrLn("Jugador " ++ show id ++ "jugando")
    sleepMs 2
    putStrLn("Jugador " ++ show id ++ "saliendo")


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)