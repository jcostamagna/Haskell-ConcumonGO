module Player where

import Control.Concurrent (threadDelay, putMVar, takeMVar, MVar, myThreadId, killThread, ThreadId, Chan, writeChan)
import Data.Foldable (for_)
import Tablero
import System.Random

dimensionTablero = 5

playerPlay id shared columna fila score = jugar
					where jugar = do 
						(columnaNew, filaNew) <- jugadas id shared columna fila score
						playerPlay id shared columnaNew filaNew score
						
						

jugadas :: Int -> MVar Tablero -> Int -> Int -> Chan (Int, IO ThreadId) -> IO (Int, Int)
jugadas id shared columna fila score = do
    
    tablero <- takeMVar shared
    sleepMs 150
    putStrLn("Jugador " ++ show id ++ " jugando")
    gen <- newStdGen
    (columnaNew, filaNew) <- posicionNueva gen shared (dejarCelda tablero columna fila) id columna fila score;
    putStrLn("Jugador " ++ show id ++ " termino turno: (" ++ show columna ++ ", " ++ show fila ++ ") ---> (" ++ show columnaNew ++ ", " ++ show filaNew ++ ")");
	return (columnaNew, filaNew)


-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)

-- sumar1 a  b = putMVar a b

posicionNueva :: StdGen -> MVar Tablero -> Tablero -> Int -> Int -> Int -> Chan (Int, IO ThreadId) -> IO (Int, Int)
posicionNueva gen shared tablero idJugador columna fila score = do
		    let (columnaNew, newGen) = randomR (columna-1, columna+1) gen;
		        (filaNew, gen2) = randomR (fila-1, fila+1) newGen;
		        filaFinal = mod filaNew dimensionTablero;
		        columnaFinal = mod columnaNew dimensionTablero;
		    bool <- verificarPos columnaFinal filaFinal shared tablero idJugador score
		    if bool
				then return (columnaFinal, filaFinal)
				else posicionNueva gen2 shared tablero idJugador columna fila score
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> Tablero -> Int -> Chan (Int, IO ThreadId) -> IO Bool
verificarPos columna fila shared tablero idJugador score = do
    let ocupado = jugador ((tablero !! columna) !! fila)
        tableroNuevo = moverseACelda tablero columna fila idJugador
        tableroFinal = matarConcumon tableroNuevo columna fila
        hayConcumon = concumon ((tablero !! columna) !! fila)
        idConcumon = concumonId ((tablero !! columna) !! fila)
    putStrLn("Jugador " ++ show idJugador ++" Busco mudarme a (" ++ show columna ++ ", " ++ show fila ++ ")");
    if ocupado
		then putStrLn("Posicion Ocupada. Vuelvo a buscar");
		else do {
                putStrLn("Tablero Inicio");
                putStrLn $ imprimirTablero tableroFinal;
                putStrLn ("Tablero Fin");
                terminarConcumon idJugador hayConcumon idConcumon score;
		        putMVar shared tableroFinal;
		        }
    return (ocupado == False);


terminarConcumon :: Int -> Bool -> ThreadId -> Chan (Int, IO ThreadId)  -> IO ()
terminarConcumon idJugador bool idConcumon score = do
	if bool
	    then do 
	            putStrLn("Concumon atrapado por jugador " ++ show idJugador ++ "!");
	            killThread idConcumon;
	            writeChan score (idJugador, myThreadId)
	            return ();
	    else return ()





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

