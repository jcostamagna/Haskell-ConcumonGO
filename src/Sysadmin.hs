module Sysadmin where

import Data.HashMap --Instalado
import Data.List (intercalate)
import Control.Concurrent ( threadDelay, Chan, ThreadId, readChan)

startSysadmin :: Int -> Chan (Int, IO ThreadId) -> IO ()
startSysadmin jugadores score = estadisticas
					where estadisticas = do let lista = [(x, 0) | x <- [1..jugadores]]
					                            map = fromList lista
					                        mostrarPuntajes map score



mostrarPuntajes :: Map Int Int -> Chan (Int, IO ThreadId) -> IO ()
mostrarPuntajes map score = do 
                               (key, threadId) <- readChan score
                               let value = findWithDefault 0 key map
                               let mapNew = insert key (value+1) map
                               let tabla = imprimirMap $ toList mapNew
                               putStrLn ("\nTabla de posiciones\n")
                               putStrLn tabla
                               sleepMs 100
                               mostrarPuntajes mapNew score
                         
                         
                         
imprimirMap :: [(Int, Int)] -> String
imprimirMap = intercalate "\n" . Prelude.map mostrarTupla


mostrarTupla :: (Int, Int) -> String
mostrarTupla (jugador, puntos) = "Jugador " ++ show jugador ++ "\t\t " ++ show puntos


sleepMs n = threadDelay (n * 1000)
