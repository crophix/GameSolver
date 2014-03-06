{-

Copyright (c) 2014 Daniel Leblanc

-}

module Negamax where

class Node a where
    nextMoves :: a -> [a]
    scoreGame :: a -> Int

data GameTree a = Leaf a
                | Branch [GameTree a]

negamax :: (Node a) => GameTree a -> Int
negamax (Leaf g)    = scoreGame g
negamax (Branch gs) = negate $ minimum (map negamax gs)



