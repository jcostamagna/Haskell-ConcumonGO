module Game where

import Control.Concurrent (forkFinally, forkIO, threadDelay, newMVar, newEmptyMVar, killThread, newChan)
import Control.Concurrent.MVar
import Data.Foldable (for_)
import Player(playerPlay)
import Tablero
import Sysadmin (startSysadmin)
import System.Random --Instalado
import NidoConcumon
import Control.Exception as E

players = 5
dimensionTablero = 5
concumons = 2

game = do{
    putStrLn $ imprimirTablero $ tablero;
    shared <- newEmptyMVar;
    score <- newChan;
    espera <- newEmptyMVar;
	putMVar shared tablero;
	forkFinally (startConcumons shared concumons espera) murioConcumon;
    startPlayers shared score;  
    startSysadmin players score;
    }
    where tablero = crearTablero dimensionTablero;

startPlayers shared score = for_ [1..players] play
    where play i = do
            putStrLn (" iniciando thread jugador " ++ show i)
            gen <- newStdGen;
            (columna, fila) <- posicionInicial gen shared i
            forkIO (playerPlay i shared columna fila score)
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
    let ocupado = jugador ((tablero !! columna) !! fila) || concumon ((tablero !! columna) !! fila)
    if ocupado
		then putMVar shared (tablero);
		else putMVar shared (moverseACelda tablero columna fila idJugador);
	return (ocupado == False);         

murioConcumon :: Either SomeException a -> IO ()
murioConcumon e = do putStrLn ("Murio Nido")
