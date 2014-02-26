{-

Copyright (c) 2014 Daniel Leblanc

-}

import Data.List
import Data.Either
import AsciiPic

-- Some data types to keep track of the game state
data Symbol     = X | O deriving (Eq, Show)
type Square     = Either Int Symbol
type Board      = [[Square]]
data GameState  = Game { board :: Board, onMove :: Symbol }

-- Starting game state
initialState :: GameState
initialState  = Game (map (map Left)[[1,2,3],[4,5,6],[7,8,9]]) X

testState :: GameState
testState  = applyMove (applyMove initialState 5) 8

-- list of current available moves
moves      :: Board -> [Int]
moves board = [x | x <- lefts (concat board)]

-- Score the current board.  Returning true if there is a victor.
scoreGame  :: Board -> Maybe Int
scoreGame board
            | tieGame board = Just 0
            | wonGame board = Just 1
            | otherwise     = Nothing

wonGame       :: Board -> Bool
wonGame board  = any match (diag board ++ board ++ transpose board)
               where match [a,b,c]  = a == b && b == c
                     diag [[a,_,b],
                           [_,c,_],
                           [d,_,e]] = [[a,c,e],[b,c,d]]

tieGame       :: Board -> Bool
tieGame board  = null (moves board)

applyMove  :: GameState -> Int -> GameState
applyMove (Game board onMove) pos
            = Game (map (map place) board) (switch onMove)
            where place (Left i) | i == pos = Right onMove
                  place x                   = x
                  switch X = O
                  switch O = X

negamax    :: GameState -> Int
negamax (Game board onmove)
            | scoreGame board == Just 0 = 0
            | scoreGame board == Just 1 = -1
            | otherwise                 = bestSc
            where state  = [applyMove (Game board onmove) x | x <- moves board]
                  bestSc = negate $ minimum (map negamax state)

bestMove     :: GameState -> GameState
bestMove game = applyMove game m
                where ms  = [(x, negamax (applyMove game x)) | x <- moves (board game)]
                      m   = fst (minimumBy cmp ms)
                      cmp (_,x) (_,y) = compare x y

picBoard      :: Board -> Pic
picBoard board = align center (map (align middle) b)
                 where b = map (map picSquare) board

picSquare         :: Square -> Pic
picSquare (Left x) = align center [tope, align middle [edge, item,edge], tope]
                     where edge = text ["|","|","|"]
                           tope = string "+-----+"
                           item = string ("  " ++ show x ++ "  ")
picSquare (Right x)= align center [tope, align middle [edge, item, edge], tope]
                     where edge = text ["|","|","|"]
                           tope = string "+-----+"
                           item = string ("  " ++ show x ++ "  ")
