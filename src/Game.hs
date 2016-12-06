module Game where

import Control.Concurrent (forkFinally, forkIO, threadDelay, writeChan, newEmptyMVar, readChan, newChan, myThreadId, Chan)
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
    myId <- myThreadId;
    shared <- newEmptyMVar;
    score <- newChan;
    espera <- newChan;
    esperaJugadores <- newChan;
	putMVar shared $ crearTablero dimensionTablero myId;
	forkFinally (startConcumons shared concumons espera) murioNido;
	forkIO (startSysadmin players score shared);
    startPlayers shared score esperaJugadores;  
    }

startPlayers shared score esperaJugadores = for_ [1..] play
    where play i = do
            putStrLn (" iniciando thread jugador " ++ show i)
            gen <- newStdGen;
            (columna, fila) <- posicionInicial gen shared i
            forkFinally (playerPlay i shared columna fila score) $ muereJugador esperaJugadores
            putStrLn (" Pos inicial de Jugador  " ++ show i ++ " (" ++ show columna ++ ", " ++ show fila ++ ")")
            if (i >= players)
               then do putStrLn ("Espero para crear mas jugadores")
                       jugador <- readChan esperaJugadores
                       sleepMs 1 
               else do sleepMs 1 
				  
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

murioNido :: Either SomeException a -> IO ()
murioNido e = do putStrLn ("Murio Nido")

muereJugador :: Chan Int -> Either SomeException a -> IO ()
muereJugador esperaJugadores e = do putStrLn ("Murio Jugador")
                                    writeChan esperaJugadores 0

sleepMs n = threadDelay (n * 1000)
