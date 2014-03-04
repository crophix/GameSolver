{-

Copyright (c) 2014 Daniel Leblanc

-}

import Data.List
import Data.Either
import AsciiPic

type Piece      = (Int, Int)
type Moves      = [(Int, Int)]
data Symbol     = H | F deriving (Eq, Show)
type Square     = Either (Int, Int) Symbol
type Board      = [[Square]]
data GameState  = Game { board  :: Board,
                         onMove :: Symbol,
                         hounds :: [Piece],
                         fox    :: Piece}

foxMoves :: Moves
foxMoves  = [(-1,-1),(1,-1),(-1,1),(1,1)]

houndMoves :: Moves
houndMoves  = [(1,-1),(1,1)]

moves         :: Piece -> Moves -> [(Int, Int)]
moves (a,b) ms = [(a+x, b+y) | (x,y) <- ms, a+x > 0,b+y > 0]

scoreGame  :: GameState -> Maybe Int
scoreGame g | null moves (fox g) foxMoves = Just p
            | fst (fox g) == 1            = Just -p
            | otherwise                   = Nothing
              where p | onMove g == H =  1
                      | otherwise     = -1

applyMove  :: GameState -> Piece -> Piece -> GameState
applyMove game start end
            = 
