module Concumon
( concumonPlay
) where



import Control.Concurrent (threadDelay, putMVar, takeMVar, ThreadId, MVar, myThreadId)
import Tablero
import System.Random
import Player (dimensionTablero)


concumonPlay id shared columna fila = jugar
					where jugar = do 
						(columnaNew, filaNew) <- jugadas id shared columna fila
						concumonPlay id shared columnaNew filaNew


jugadas :: Int -> MVar Tablero -> Int -> Int -> IO (Int, Int)
jugadas id shared columna fila = do
    sleepMs 2
    tablero <- takeMVar shared
    putStrLn("Concumon " ++ show id ++ " moviendose")
    gen <- newStdGen
    (columnaNew, filaNew) <- posicionNueva gen shared (dejarCeldaConcumon tablero columna fila) myThreadId columna fila;
    putStrLn("Concumon " ++ show id ++ " termino turno: (" ++ show columna ++ ", " ++ show fila ++ ") ---> (" ++ show columnaNew ++ ", " ++ show filaNew ++ ")");
	return (columnaNew, filaNew)
	
	
posicionNueva :: StdGen -> MVar Tablero -> Tablero -> IO ThreadId -> Int -> Int -> IO (Int, Int)
posicionNueva gen shared tablero idConcumon columna fila = do
		    let (columnaNew, newGen) = randomR (columna-1, columna+1) gen;
		        (filaNew, gen2) = randomR (fila-1, fila+1) newGen;
		        filaFinal = mod filaNew dimensionTablero;
		        columnaFinal = mod columnaNew dimensionTablero;
		    bool <- verificarPos columnaFinal filaFinal shared tablero idConcumon
		    if bool
				then return (columnaFinal, filaFinal)
				else posicionNueva gen2 shared tablero idConcumon columna fila
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> Tablero -> IO ThreadId -> IO Bool
verificarPos columna fila shared tablero idConcumon = do
    let ocupado = jugador ((tablero !! columna) !! fila) && concumon ((tablero !! columna) !! fila)
        tableroNuevo = moverseACeldaConcumon tablero columna fila idConcumon
    putStrLn("Concumon: Busco mudarme a (" ++ show columna ++ ", " ++ show fila ++ ")");
    if ocupado
		then putStrLn("Posicion Ocupada. Vuelvo a buscar");
		else do {
                putStrLn("Tablero Inicio");
                putStrLn $ imprimirTablero tableroNuevo;
                putStrLn ("Tablero Fin");
		        putMVar shared tableroNuevo;
		        }
    return (ocupado == False);
    
    
sleepMs n = threadDelay (n * 1000)
    
    
    
    
    
    
