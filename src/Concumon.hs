module Concumon
( concumonPlay
) where



import Control.Concurrent (threadDelay, putMVar, takeMVar, ThreadId, MVar, myThreadId)
import Tablero
import System.Random
import Control.Exception
import System.Timeout

dimensionTablero = 5

concumonPlay :: Int -> MVar Tablero -> IO ()
concumonPlay id shared = do putStrLn ("Iniciando thread concumon " ++ show id)
                            tablero <- takeMVar shared
                            gen <- newStdGen;
                            (columna, fila, tableroNuevo) <- posicionInicial gen tablero
                            putMVar shared (tableroNuevo)
                            putStrLn ("Concumon: Pos inicial  " ++ show columna ++ " " ++ show fila)
                            jugar id shared columna fila


jugar id shared columna fila = do 
           (columnaNew, filaNew) <- jugadas id shared columna fila
           --sleepMs 30
           jugar id shared columnaNew filaNew


jugadas :: Int -> MVar Tablero -> Int -> Int -> IO (Int, Int)
jugadas id shared columna fila = do
    tablero <- takeMVar shared
    --sleepMs 150
    putStrLn("Concumon " ++ show id ++ " moviendose")
    gen <- newStdGen
    idConcumon <- myThreadId
    (columnaNew, filaNew) <- posicionNueva gen shared (dejarCeldaConcumon tablero columna fila) idConcumon columna fila;
    putStrLn("Concumon " ++ show id ++ " termino turno: (" ++ show columna ++ ", " ++ show fila ++ ") ---> (" ++ show columnaNew ++ ", " ++ show filaNew ++ ")");
	return (columnaNew, filaNew)
	
	
	
posicionNueva :: StdGen -> MVar Tablero -> Tablero -> ThreadId -> Int -> Int -> IO (Int, Int)
posicionNueva gen shared tablero idConcumon columna fila = do
		    let (columnaNew, newGen) = randomR (columna-1, columna+1) gen;
		        (filaNew, gen2) = randomR (fila-1, fila+1) newGen;
		        filaFinal = mod filaNew dimensionTablero;
		        columnaFinal = mod columnaNew dimensionTablero;
		    bool <- verificarPos columnaFinal filaFinal shared tablero idConcumon
		    if bool
				then return (columnaFinal, filaFinal)
				else posicionNueva gen2 shared tablero idConcumon columna fila
		      
		
verificarPos :: Int -> Int -> MVar Tablero -> Tablero -> ThreadId -> IO Bool
verificarPos columna fila shared tablero idConcumon = do
    let ocupadoJug = jugador ((tablero !! columna) !! fila)
        ocupadoCon = concumon ((tablero !! columna) !! fila)
        ocupado = ocupadoJug || ocupadoCon
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
    
    
posicionInicial :: StdGen -> Tablero -> IO (Int, Int, Tablero)
posicionInicial gen tablero = do
		    let (columna, newGen) = randomR (0, dimensionTablero-1) gen;
		        (fila, gen2) = randomR (0, dimensionTablero-1) newGen;
		    idConcumon <- myThreadId
		    (bool, tableroNuevo) <- verificarPosInicial columna fila tablero idConcumon
		    if bool
				then return (columna,fila, tableroNuevo)
				else posicionInicial gen2 tablero
		      
		
verificarPosInicial :: Int -> Int -> Tablero -> ThreadId -> IO (Bool, Tablero)
verificarPosInicial columna fila tablero idConcumon = do
    let ocupado = jugador ((tablero !! columna) !! fila) || concumon ((tablero !! columna) !! fila)
    if ocupado
		then return (ocupado == False, tablero);
		else return (ocupado == False, moverseACeldaConcumon tablero columna fila idConcumon); 
    
    
sleepMs n = threadDelay (n * 1000)
    
    
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

dangerous :: IO Int
dangerous = do
    putStrLn "Succeeds this time, but takes some time"
    threadDelay 10000
    return 5
    
    
