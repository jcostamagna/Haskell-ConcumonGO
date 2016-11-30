module Game where

import Control.Concurrent (forkIO, threadDelay, newMVar, newEmptyMVar, killThread)
import Control.Concurrent.MVar
import Data.Foldable (for_)
import Player(playerPlay)
import Tablero
import System.Random --Instalado
import NidoConcumon

players = 5

concumons = 2

game = do{
    putStrLn $ imprimirTablero $ tablero;
    shared <- newEmptyMVar;
	putMVar shared tablero;
    startPlayers shared;
    forkIO (startConcumons shared concumons);
    }
    where tablero = crearTablero dimensionTablero;

startPlayers shared = for_ [1..players] play
    where play i = do
            putStrLn (" iniciando thread jugador " ++ show i)
            gen <- newStdGen;
            (columna, fila) <- posicionInicial gen shared i
            forkIO (playerPlay i shared columna fila)
            putStrLn (" Pos inicial  " ++ show columna ++ " " ++ show fila) 
				  
posicionInicial :: StdGen -> MVar Tablero -> Int -> IO (Int, Int)
posicionInicial gen shared idJugador = do
		    let (columna, newGen) = randomR (0, dimensionTablero-1) gen;
		        (fila, gen2) = randomR (0, dimensionTablero-1) newGen;
		    bool <- verificarPos columna fila shared idJugador
		    if bool
				then return (columna,fila)
				else posicionInicial gen2 shared idJugador
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> Int -> IO Bool
verificarPos columna fila shared idJugador = do
    tablero <- takeMVar shared;
    putStrLn $ imprimirTablero $ tablero;
    let ocupado = jugador ((tablero !! columna) !! fila) && concumon ((tablero !! columna) !! fila)
    if ocupado
		then putMVar shared (tablero);
		else putMVar shared (moverseACelda tablero columna fila idJugador);
	return (ocupado == False);         


