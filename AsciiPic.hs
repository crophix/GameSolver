module AsciiPic where

import Data.List

data Pic = Pic { height :: Int,
                 width  :: Int,
                 img    :: [String] }

render              :: Pic -> IO ()
render (Pic h w img) = putStr (unlines img)

string  :: String -> Pic
string s = Pic {height = 1, width = length s, img = [s] }
 
text    :: [String] -> Pic
text     = align left . map string

block      :: Int -> Int -> Char -> Pic
block h w c = Pic h w (replicate h (replicate w c))

blank    :: Int -> Int -> Pic
blank h w = block h w ' '

hspace  :: Int -> Pic
hspace w = blank 0 w

vspace  :: Int -> Pic
vspace h = blank h 0

empty :: Pic
empty  = Pic 0 0 []

hflip              :: Pic -> Pic
hflip (Pic h w img) = Pic h w (reverse img)

vflip              :: Pic -> Pic
vflip (Pic h w img) = Pic h w (map reverse img)

rot90                :: Pic -> Pic
rot90 (Pic h w img)   | h == 0 || w == 0 = blank w h 
                      | otherwise = Pic h w (transpose (reverse img))

rot180               :: Pic -> Pic
rot180 (Pic h w img)  = Pic h w (reverse (map reverse img))

rot270               :: Pic -> Pic
rot270 (Pic h w img)  | h == 0 || w == 0 = blank w h
                      | otherwise = Pic h w (reverse (transpose img))

infixr `left`, `right`, `center`

left, right, center               :: Pic -> Pic -> Pic
left  (Pic h1 w1 t) (Pic h2 w2 b)  = Pic (h1+h2) w (map (rpad w) t ++ map (rpad w) b)
                                   where w = max w1 w2

right (Pic h1 w1 t) (Pic h2 w2 b)  = Pic (h1+h2) w (map (lpad w) t ++ map (lpad w) b)
                                   where w = max w1 w2

center (Pic h1 w1 t) (Pic h2 w2 b) = Pic (h1+h2) w (map (cpad w) t ++ map (cpad w) b)
                                   where w = max w1 w2

lpad     :: Int -> String -> String
lpad n s  = reverse (rpad n (reverse s))

rpad     :: Int -> String -> String
rpad n s  = take n (s ++ repeat ' ')

cpad     :: Int -> String -> String
cpad n s  = lpad lw (take split s) ++ rpad (n-lw) (drop split s)
            where split = length s `div` 2
                  lw    = n `div` 2

align   :: (Pic -> Pic -> Pic) -> [Pic] -> Pic
align op = foldr op empty

infixr `top`, `bottom`, `middle`

top, bottom, middle  :: Pic -> Pic -> Pic
top    = horiz bpad
bottom = horiz tpad
middle = horiz mpad
 
horiz :: (Int -> Int -> [String] -> [String]) -> Pic -> Pic -> Pic
horiz pad (Pic lh lw limg) (Pic rh rw rimg)
   | lh > rh   = Pic lh w (zipWith (++) limg (pad lh rw rimg))
   | lh < rh   = Pic rh w (zipWith (++) (pad rh lw limg) rimg)
   | otherwise = Pic lh w (zipWith (++) limg rimg)
     where w        = lw + rw

tpad         :: Int -> Int -> [String] -> [String]
tpad h w s    = reverse (bpad h w (reverse s))

bpad         :: Int -> Int -> [String] -> [String]
bpad h w s    = take h (s ++ repeat (replicate w ' '))

mpad         :: Int -> Int -> [String] -> [String]
mpad h w s    = tpad th w (take split s) ++ bpad (h-th) w (drop split s)
                where split = length s `div` 2
                      th    = h `div` 2


hborder    :: Int -> Pic -> Pic
hborder n p = b `top` p `top` b
              where b = block (height p) n ' '

vborder    :: Int -> Pic -> Pic
vborder n p = b `left` p `left` b
              where b = block n (width p) ' '

vpad, hpad :: Int -> (Pic -> Pic -> Pic) -> Pic -> Pic
vpad n op p = vspace n `op` p
hpad n op p = hspace n `op` p

border  :: Int -> Pic -> Pic
border n = hborder n . vborder n

