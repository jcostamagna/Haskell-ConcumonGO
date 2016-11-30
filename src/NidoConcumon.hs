module NidoConcumon
(
startConcumons
, dimensionTablero
) where


import Control.Concurrent (forkIO, threadDelay, newMVar, newEmptyMVar, ThreadId, myThreadId)
import Control.Concurrent.MVar
import Tablero
import Data.Foldable (for_)
import System.Random
import Concumon


dimensionTablero = 5

startConcumons :: MVar Tablero -> Int -> IO ()
startConcumons shared concumons= for_ [1..concumons] play
    where play i = do
            putStrLn (" iniciando thread concumon " ++ show i)
            gen <- newStdGen;
            (columna, fila) <- posicionInicial gen shared i
            forkIO (concumonPlay i shared columna fila)
            putStrLn ("Concumon: Pos inicial  " ++ show columna ++ " " ++ show fila) 


posicionInicial :: StdGen -> MVar Tablero -> Int -> IO (Int, Int)
posicionInicial gen shared idJugador = do
		    let (columna, newGen) = randomR (0, dimensionTablero-1) gen;
		        (fila, gen2) = randomR (0, dimensionTablero-1) newGen;
		    bool <- verificarPos columna fila shared myThreadId --Cuidado!!
		    if bool
				then return (columna,fila)
				else posicionInicial gen2 shared idJugador
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> IO ThreadId -> IO Bool
verificarPos columna fila shared idConcumon = do
    tablero <- takeMVar shared;
    putStrLn $ imprimirTablero $ tablero;
    let ocupado = jugador ((tablero !! columna) !! fila) && concumon ((tablero !! columna) !! fila)
    if ocupado
		then putMVar shared (tablero);
		else putMVar shared (moverseACeldaConcumon tablero columna fila idConcumon);
	return (ocupado == False);  
    
    
    
    
    
    
