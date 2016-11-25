module Player where

import Control.Concurrent (threadDelay, putMVar, takeMVar, MVar)
import Data.Foldable (for_)

playerPlay id shared = for_ (takeWhile (<3) [1..]) jugar
					where jugar i = do 
						jugadas id shared
--graceQuit shared

graceQuit :: MVar Int -> Int -> Bool
graceQuit shared i
	|bool <- (verificar shared) = True
	| otherwise = False
		
verificar :: MVar Int -> IO Bool
verificar shared = do{
		v <- takeMVar shared; 
		putMVar shared v;
		putStrLn("Esssssssss " ++ show v );
		return $ v < 5	
		}
		
terminar :: Int -> Bool
terminar v
    | v > 1   = True
    | otherwise = False


jugadas id shared = do
    putStrLn("Jugador " ++ show id ++ "jugando")
    sleepMs 2
    var <- takeMVar shared
    putStrLn("Jugador " ++ show id ++ "saliendo " ++ show var )
    putMVar shared (var+1)


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

-- sumar1 a  b = putMVar a b

inc :: MVar Int -> IO()
inc shared = do {
		v <- takeMVar shared; 
		putMVar shared (v+1)	
		}

