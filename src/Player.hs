module Player where

import Control.Concurrent (threadDelay, putMVar, takeMVar, MVar, myThreadId)
import Data.Foldable (for_)
import Tablero
import System.Random

dimensionTablero = 5

playerPlay id shared columna fila = jugar
					where jugar = do 
						(columnaNew, filaNew) <- jugadas id shared columna fila
						playerPlay id shared columnaNew filaNew
						
						

jugadas :: Int -> MVar Tablero -> Int -> Int -> IO (Int, Int)
jugadas id shared columna fila = do
    sleepMs 2
    tablero <- takeMVar shared
    putStrLn("Jugador " ++ show id ++ " jugando")
    gen <- newStdGen
    (columnaNew, filaNew) <- posicionNueva gen shared (dejarCelda tablero columna fila) id columna fila;
    putStrLn("Jugador " ++ show id ++ " termino turno: (" ++ show columna ++ ", " ++ show fila ++ ") ---> (" ++ show columnaNew ++ ", " ++ show filaNew ++ ")");
	return (columnaNew, filaNew)


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

-- sumar1 a  b = putMVar a b

posicionNueva :: StdGen -> MVar Tablero -> Tablero -> Int -> Int -> Int -> IO (Int, Int)
posicionNueva gen shared tablero idJugador columna fila = do
		    let (columnaNew, newGen) = randomR (columna-1, columna+1) gen;
		        (filaNew, gen2) = randomR (fila-1, fila+1) newGen;
		        filaFinal = mod filaNew dimensionTablero;
		        columnaFinal = mod columnaNew dimensionTablero;
		    bool <- verificarPos columnaFinal filaFinal shared tablero idJugador
		    if bool
				then return (columnaFinal, filaFinal)
				else posicionNueva gen2 shared tablero idJugador columna fila
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> Tablero -> Int -> IO Bool
verificarPos columna fila shared tablero idJugador = do
    let ocupado = jugador ((tablero !! columna) !! fila)
        tableroNuevo = moverseACelda tablero columna fila idJugador
    putStrLn("Jugador " ++ show idJugador ++" Busco mudarme a (" ++ show columna ++ ", " ++ show fila ++ ")");
    if ocupado
		then putStrLn("Posicion Ocupada. Vuelvo a buscar");
		else do {
                putStrLn("Tablero Inicio");
                putStrLn $ imprimirTablero tableroNuevo;
                putStrLn ("Tablero Fin");
		        putMVar shared tableroNuevo;
		        }
    return (ocupado == False);







inc :: MVar Int -> IO()
inc shared = do {
		v <- takeMVar shared; 
		putMVar shared (v+1)	
		}
		
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

