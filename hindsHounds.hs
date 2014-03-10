{-

Copyright (c) 2014 Daniel Leblanc

-}

import Data.List
import Data.Either
import AsciiPic

type Position   = (Int, Int)
type Moves      = [(Int, Int)]
data Symbol     = Empty | H | F deriving (Eq, Show)
data FoxHounds  = Game { board  :: [(Position,Symbol)],
                         onMove :: Symbol,
                         hounds :: [Position],
                         fox    :: Position}

foxMoves   :: Moves --Diagonal in any direction
foxMoves    = [(-1,-1),(1,-1),(-1,1),(1,1)]
houndMoves :: Moves --Diagonal forward only
houndMoves  = [(1,-1),(1,1)]

moves           :: Position -> [Position] -> Moves -> [(Int, Int)]
moves (a,b) o ms = [(a+x, b+y) | (x,y) <- ms, 
                                 a+x > 0 && a+x < 9,
                                 b+y > 0 && b+y < 9, 
                                 not ((a+x,b+y) `elem` o)]

scoreGame  :: FoxHounds -> Int
scoreGame g | null (moves (fox g) (hounds g) foxMoves) =  p
            | fst (fox g) == 1                         = -p
            | otherwise                                =  0
              where p | onMove g == H =  1
                      | otherwise     = -1

gameOver  :: FoxHounds -> Bool
gameOver g = null (moves (fox g) (hounds g) foxMoves) || 
             (and $ map null [moves s [fox g] houndMoves | s <- hounds g]) ||
             fst (fox g) == 1

switchPlayer  :: Symbol -> Symbol
switchPlayer p | p == H    = F
               | otherwise = H

applyMove  :: FoxHounds -> Position -> Position -> FoxHounds
applyMove g start end
            = Game (map (clear . place) (board g)) (switchPlayer $ onMove g) h f
              where clear a | fst a == start = (start,Empty)
                            | otherwise      = a
                    place a | fst a == end   = (end, onMove g)
                            | otherwise      = a
                    h | onMove g == H = end : [x | x <- hounds g, x /= start]
                      | otherwise     = hounds g
                    f | onMove g == F = end
                      | otherwise     = fox g
 
allMoves  :: FoxHounds -> [FoxHounds]
allMoves g | onMove g == H = [applyMove g s e | s <- hounds g, e <- moves s [fox g] houndMoves]
           | otherwise     = [applyMove g (fox g) e | e <- moves (fox g) (hounds g) foxMoves]

negamax  :: FoxHounds -> Int
negamax g | result /= 0  = result
          | null ms      = result
          | otherwise    = maximum (map (negate . negamax) ms)
            where result = scoreGame g
                  ms     = allMoves g


emptyBoard :: [(Position, Symbol)]
emptyBoard  = [((a,b),Empty) | a <- [1..8], b <- [1..8]]

startState :: FoxHounds
startState  = Game emptyBoard F [(1,1),(1,3),(1,5),(1,7)] (8,6)

fwin, hwin :: FoxHounds
fwin  = Game emptyBoard F [(2,6),(2,4),(3,5),(4,6)] (1,1)
hwin  = Game emptyBoard H [(8,6),(7,7),(6,8),(1,1)] (8,8)

fillPos           :: [(Position,Symbol)] -> [Position] -> Symbol -> [(Position,Symbol)]
fillPos b [] s     = b
fillPos b (p:ps) s = fillPos (map (place p s) b) ps s
                     where place p s (l,e) | l == p    = (l,s)
                                           | otherwise = (l,e)

tState1 :: FoxHounds
tState1 = Game (fillPos (fillPos emptyBoard h H) [f] F) F h f
          where h = [(1,1),(8,6),(6,8),(8,4)]
                f = (2,8)


picSquare          :: (Position,Symbol) -> Pic
picSquare ((a,b),x) = align center [tope, align middle [edge, item]]
                      where edge = text ["|"]
                            tope = string "+---"
                            item = string (r x)
                            r Empty | even (a+b) = "   "
                                    | odd  (a+b) = "###"
                            r t                  = " " ++ show x ++ " "

picBoard   :: [(Position,Symbol)] -> Pic
picBoard ps = align center ([picRow x | x <- splitEvery 8 ps] ++ 
                            [string "+---+---+---+---+---+---+---+---+"])

picRow ps = align middle ((map picSquare ps) ++ [text ["+","|"]])

splitEvery     :: Int -> [a] -> [[a]]
splitEvery i ls = map (take i) (build (splitter ls))
                  where splitter [] _ n = n
                        splitter l c n  = l `c` splitter (drop i l) c n
                        build g         = g (:) []
