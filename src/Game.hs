module Game where

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import Player(playerPlay)

players = 5

game = do 
    start

start = for_ [1..players] play
    where play i = do
            putStrLn (" player " ++ show i)
            forkIO (playerPlay i)



--printMessagesFrom "hola"


--printMessagesFrom name = for_ [1..3] printMessage
  --  where printMessage i = do
     --       putStrLn (name ++ " number " ++ show i)