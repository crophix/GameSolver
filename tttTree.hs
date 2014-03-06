{-

Copyright (c) 2014 Daniel Leblanc

-}

import Negamax

data Symbol    =  Empty | X | O deriving (Eq, Show)
data TicTacToe = Game { board :: [[Symbol]], onMove :: Symbol }

instance Node TicTacToe where
    nextMoves a = [a]
    scoreGame _ = 1
