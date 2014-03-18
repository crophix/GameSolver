{-

Copyright (c) 2014 Daniel Leblanc

-}

module Negamax where


negamax    :: (a -> Bool) -> (a -> [a]) -> (a -> Int) -> a -> Int
negamax over moves eval game 
            | over game = eval game
            | otherwise = maximum (map 
                            (negate . (negamax over moves eval)) 
                            (moves game))



