module Game where

import Control.Concurrent (forkIO, threadDelay, newMVar, newEmptyMVar)
import Control.Concurrent.MVar
import Data.Foldable (for_)
import Player(playerPlay)

players = 5



game = do{
	shared <- newEmptyMVar;
	putMVar shared 0;
    startPlayers shared}

startPlayers shared = for_ [1..players] play
    where play i = do
            putStrLn (" iniciando thread jugador " ++ show i)
            forkIO (playerPlay i shared)
