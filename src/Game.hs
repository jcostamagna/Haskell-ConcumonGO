module Game where

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import Player(playerPlay)

players = 5

game = do 
    startPlayers

startPlayers = for_ [1..players] play
    where play i = do
            putStrLn (" iniciando thread jugador " ++ show i)
            forkIO (playerPlay i)
