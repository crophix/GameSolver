{-

Copyright (c) 2014 Daniel Leblanc

-}

-- import Negamax
import Data.List(transpose)
import AsciiPic

data Symbol    = Empty Int | X | O deriving (Eq, Show)
data TicTacToe = Game { board :: [[Symbol]], onMove :: Symbol }
                 deriving Show
{-
instance Node TicTacToe where
    nextMoves = applyMoves
    scoreGame = gameEval
-}

switchPlayer  :: Symbol -> Symbol
switchPlayer X = O
switchPlayer O = X

applyMoves  :: TicTacToe -> [TicTacToe]
applyMoves g | gameOver g = []
             | otherwise  =[makeMove g s | s <- moves g]

moves  :: TicTacToe -> [Symbol]
moves g = [x | x <- concat (board g), x /= X, x /= O]

makeMove    :: TicTacToe -> Symbol -> TicTacToe
makeMove g s = Game (map (map place) (board g)) p
               where p = switchPlayer (onMove g)
                     place b | b == s    = onMove g
                             | otherwise = b

gameOver  :: TicTacToe -> Bool
gameOver g = (not $ null (wonGame g)) || tieGame g

wonGame  :: TicTacToe -> [Symbol]
wonGame g = [x | [x,y,z] <- lines, x == y && y == z]
            where diag [[a,_,b],
                        [_,c,_],
                        [d,_,e]] = [[a,c,e],[b,c,d]]
                  brd   = board g
                  lines = brd ++ diag brd ++ transpose brd

gameEval  :: TicTacToe -> Int
gameEval g | tieGame g               =  0
           | null winner             =  1
           | head winner == onMove g =  3
           | otherwise               = -3
             where winner = wonGame g

tieGame :: TicTacToe -> Bool
tieGame  = null . moves

initialState :: TicTacToe
initialState  = Game [[Empty 1,Empty 2,Empty 3],
                      [Empty 4,Empty 5,Empty 6],
                      [Empty 7,Empty 8,Empty 9]] X
testState1 :: TicTacToe
testState1  = Game [[Empty 1,O      ,Empty 3],
                    [Empty 4,X      ,Empty 6],
                    [Empty 7,X      ,Empty 9]] O

testState2 :: TicTacToe
testState2  = Game [[      O, X,Empty 3],
                    [Empty 4, O,Empty 6],
                    [Empty 7, X,Empty 9]] X

testState3 :: TicTacToe
testState3  = Game [[      O, X,Empty 3],
                    [Empty 4, O,Empty 6],
                    [      X, X,Empty 9]] O

testState4 :: TicTacToe
testState4  = Game [[O,X,O],
                    [O,X,X],
                    [X,O,X]] O


negamax  :: TicTacToe -> Int
negamax g | gameOver g = gameEval g
          | otherwise  = maximum (map (negate . negamax) (applyMoves g))

picGame       :: TicTacToe -> Pic
picGame g      = picBoard (board g)       

picBoard      :: [[Symbol]] -> Pic
picBoard board = align center (map (align middle) b)
                 where b = map (map picSquare) board

picSquare     :: Symbol -> Pic
picSquare x    = align center [tope, align middle [edge, item,edge], tope]
                 where edge = text ["|","|","|"]
                       tope = string "+-----+"
                       item = string ("  " ++ (r x) ++ "  ")
                       r (Empty n) = show n
                       r a         = show x
