module Tablero
( Celda(..)
, Tablero(..)
,crearTablero
, imprimirCelda
, imprimirTablero
, moverseACelda
, dejarCelda
) where

import Data.List (intercalate)
import Control.Concurrent (ThreadId, myThreadId)


data Celda =  Celda { 
					jugador :: Bool,
					jugadorId :: Int,
					concumon :: Bool,
					concumonId :: IO ThreadId
					}
					
imprimirCelda :: Celda -> String
imprimirCelda (Celda {jugador = a, jugadorId = b,  concumon = c, concumonId = cId})
		| a = show b
		| c = "*"
		| otherwise = "-"
		
imprimirCeldas :: [Celda] -> String
imprimirCeldas = intercalate " " . map imprimirCelda 
		

type Tablero = [[Celda]]

imprimirTablero :: Tablero -> String
imprimirTablero = intercalate "\n" . map imprimirCeldas 


					
crearTablero :: Int -> Tablero
crearTablero n = replicate n $ replicate n $ Celda False 0 False myThreadId

moverseACelda :: Tablero -> Int -> Int -> Int -> Tablero
moverseACelda tablero columna fila id = 
	take columna tablero ++
	[take fila (tablero !! columna) ++ [((tablero !! columna) !! fila) {jugador = True, jugadorId = id}] ++ drop (fila + 1) (tablero !! columna)] ++
	drop (columna + 1) tablero

dejarCelda :: Tablero -> Int -> Int -> Tablero
dejarCelda tablero columna fila = 
	take columna tablero ++
	[take fila (tablero !! columna) ++ [((tablero !! columna) !! fila) {jugador = False}] ++ drop (fila + 1) (tablero !! columna)] ++
	drop (columna + 1) tablero
	
	

