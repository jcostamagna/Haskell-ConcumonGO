module Player where

import Control.Concurrent (threadDelay, putMVar, takeMVar, MVar, myThreadId)
import Data.Foldable (for_)
import Tablero

playerPlay id shared = for_ [1..] jugar
					where jugar i = do 
						jugadas id shared
						
--graceQuit shared


graceQuit :: MVar Int -> Int -> Bool
graceQuit shared i
	| bool <- (verificar shared) = True
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
    tablero <- takeMVar shared
    putStrLn("Tablero Inicio")
    putStrLn $ imprimirTablero $ tablero;
    putStrLn("Tablero Fin");
    --tablero ++ [Celda False False myThreadId]
    --let celda = moverseACelda ((tablero !! id) !! id)
    --putStrLn("Jugador " ++ show id ++ "saliendo " ++ show var )
    --putStrLn $ imprimirCelda celda
    --putStrLn $ imprimirCelda celda
    --((var !! id) !! id).{jugador = True}
    --return $ map (\x -> map (\y -> celda {jugador = True}) y) tablero
    --let seq = update id celda $ fromList (tablero !! id);
	let num = id -1;
    putMVar shared (tablero)


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

-- sumar1 a  b = putMVar a b

inc :: MVar Int -> IO()
inc shared = do {
		v <- takeMVar shared; 
		putMVar shared (v+1)	
		}

