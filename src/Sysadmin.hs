module Sysadmin where

import Data.HashMap --Instalado
import Data.List (intercalate)
import Control.Concurrent
import Tablero

startSysadmin :: Int -> Chan (Int, ThreadId, Int, Int) -> MVar Tablero -> IO ()
startSysadmin jugadores score tablero = estadisticas
					where estadisticas = do let lista = [(x, 0) | x <- [1..jugadores]]
					                            map = fromList lista
					                        mostrarPuntajes map score tablero



mostrarPuntajes :: Map Int Int -> Chan (Int, ThreadId, Int, Int) -> MVar Tablero -> IO ()
mostrarPuntajes map score tableroShared = do 
                               tablero <- takeMVar tableroShared
                               
                               (mapNew, tableroNuevo) <- sumarPuntajes map score tablero
                               let tabla = imprimirMap $ toList mapNew
                               putStrLn ("\nTabla de posiciones\n")
                               putStrLn tabla
                               putMVar tableroShared tableroNuevo;
                               mostrarPuntajes mapNew score tableroShared
                               
                               
sumarPuntajes :: Map Int Int -> Chan (Int, ThreadId, Int, Int) -> Tablero -> IO (Map Int Int, Tablero)                             
sumarPuntajes map score tablero = do
                                    estaVacio <- isEmptyChan score
                                    if estaVacio
                                        then do return (map, tablero)
                                        else do 
                                                (key, threadId, columna, fila) <- readChan score
                                                let value = findWithDefault 0 key map
                                                let mapNew = insert key (value+1) map
                                                if (value+1) >= 2
                                                    then do killThread threadId
                                                            sumarPuntajes mapNew score $ dejarCelda tablero columna fila
                                                    else do sumarPuntajes mapNew score tablero
                         
                         
                         
imprimirMap :: [(Int, Int)] -> String
imprimirMap = intercalate "\n" . Prelude.map mostrarTupla


mostrarTupla :: (Int, Int) -> String
mostrarTupla (jugador, puntos) = "Jugador " ++ show jugador ++ "\t\t " ++ show puntos


sleepMs n = threadDelay (n * 1000)
