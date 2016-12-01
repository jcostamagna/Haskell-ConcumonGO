module NidoConcumon
(
startConcumons
) where


import Control.Concurrent (forkFinally, threadDelay, newMVar, newEmptyMVar, ThreadId, myThreadId)
import Control.Concurrent.MVar
import Control.Exception as E
import Tablero
import Data.Foldable (for_)
import System.Random
import Concumon (concumonPlay)




startConcumons :: MVar Tablero -> Int -> MVar Int -> IO ()
startConcumons shared concumons espera = for_ [1..] $ play shared concumons espera
    
    
play :: MVar Tablero -> Int -> MVar Int -> Int -> IO ()
play shared concumons espera i = do 
            forkFinally (concumonPlay i shared) $ murioConcumon espera          
            sleepMs 1000
            if (i >= concumons)
               then do --espero <- takeMVar espera
                       sleepMs 5
               else do sleepMs 5



     
     
murioConcumon :: MVar Int -> Either SomeException a -> IO ()
murioConcumon espera e = do putStrLn ("Murio Concumon")
                            --putMVar espera 0
    
sleepMs n = threadDelay (n * 1000)

    
