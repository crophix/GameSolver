{- 

Copyright (c) 2014 Daniel Leblanc

-}

import Data.List(transpose)

data Symbol     = Empty Int | X | O deriving (Eq, Show)
data QuantumTTT = Game { board  :: [[Symbol]],
                         onMove :: Symbol,
                         cMoves :: [(Int, Int)] }

switchPlayer  :: Symbol -> Symbol
switchPlayer X = O
switchPlayer O = X

applyMoves  :: QuantumTTT -> [QuantumTTT]
applyMoves g | gameOver g = []
             | otherwise  = concat [makeMove g s | s <- moves g]

moves  :: QuantumTTT -> [(Int,Int)]
moves g = [(x,y) | Empty x <- b, Empty y <- b, x < y]
          where b = [x | x <- concat (board g), x /= O, x /= X]

makeMove    :: QuantumTTT -> (Int,Int) -> [QuantumTTT]
makeMove g s = Game (board g) p (s: cMoves g)
               where p = switchPlayer (onMove g)

observe  :: QuantumTTT -> [QuantumTTT]
observe g = Error "crap"

gameOver  :: QuantumTTT -> Bool
gameOver g = (not $ null (wonGame g)) || tieGame g

wonGame  :: QuantumTTT -> [Symbol]
wonGame g = [x | [x,y,z] <- lines, x == y && y == z]
            where diag [[a,_,b],
                        [_,c,_],
                        [d,_,e]] = [[a,c,e],[b,c,d]]
                  brd   = board g
                  lines = brd ++ diag brd ++ transpose brd

gameEval  :: QuantumTTT -> Int
gameEval g | tieGame g               =  0
           | null winner             =  1
           | head winner == onMove g =  3
           | otherwise               = -3
             where winner = wonGame g

tieGame :: QuantumTTT -> Bool
tieGame  = null . moves

initialState :: QuantumTTT
initialState  = Game [[Empty 1,Empty 2,Empty 3],
                      [Empty 4,Empty 5,Empty 6],
                      [Empty 7,Empty 8,Empty 9]] X []

negamax  :: QuantumTTT -> Int
negamax g | gameOver g = gameEval g
          | otherwise  = maximum (map (negate . negamax) (applyMoves g))


