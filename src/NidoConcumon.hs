module NidoConcumon
(
startConcumons
) where


import Control.Concurrent (forkFinally, threadDelay, Chan, readChan, ThreadId, writeChan)
import Control.Concurrent.MVar
import Control.Exception as E
import Tablero
import Data.Foldable (for_)
import System.Random
import Concumon (concumonPlay)




startConcumons :: MVar Tablero -> Int -> Chan Int -> IO ()
startConcumons shared concumons espera = for_ [1..] $ play shared concumons espera
    
    
play :: MVar Tablero -> Int -> Chan Int -> Int -> IO ()
play shared concumons espera i = do 
            forkFinally (concumonPlay i shared) $ murioConcumon espera          
            sleepMs 1000
            if (i >= concumons)
               then do putStrLn ("Espero para crear mas concumons")
                       concumon <- readChan espera
                       sleepMs 5
               else do sleepMs 5



     
     
murioConcumon :: Chan Int -> Either SomeException a -> IO ()
murioConcumon espera e = do putStrLn ("Murio Concumon")
                            writeChan espera 0
    
sleepMs n = threadDelay (n * 1000)

    
