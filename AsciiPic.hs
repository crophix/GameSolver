module AsciiPic where

import Data.List

data Pic = Pic { height :: Int,
                 width  :: Int,
                 img    :: [String] }

render              :: Pic -> IO ()
render (Pic h w img) = putStr (unlines img)

string  :: String -> Pic
string s = Pic {height = 1, width = length s, img = [s] }

block      :: Int -> Int -> Char -> Pic
block h w c = Pic h w (replicate h (replicate w c))

blank    :: Int -> Int -> Pic
blank h w = block h w ' '

hspace  :: Int -> Pic
hspace w = blank 0 w

vspace  :: Int -> Pic
vspace h = blank h 0

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
lpad n s  | n == length s = s
          | n < length s  = lpad n (tail s)
          | otherwise     = lpad n (' ':s)

rpad     :: Int -> String -> String
rpad n s  | n == length s = s
          | n < length s  = take n s
          | otherwise     = rpad n (s ++ " ")

cpad     :: Int -> String -> String
cpad n s  | n == length s = s
          | n > length s  = cpad n (" " ++ s ++ " ")
          | otherwise     = cstrip n (splitAt (quot (length s) 2) s)

cstrip          :: Int -> (String, String) -> String
cstrip n (a,b)   = reverse (take (quot n 2) (reverse a)) ++ (take (n - (quot n 2)) b)
 
align            :: (Pic -> Pic -> Pic) -> [Pic] -> Pic
align f (a:[])    = a
align f (a:b:ps)  = align f (f a b : ps)

text :: [String] -> Pic
text  = align left . map string

infixr `top`, `bottom`, `middle`

top, bottom, middle  :: Pic -> Pic -> Pic

top (Pic h1 w1 i1) (Pic h2 w2 i2)    
                      = Pic h (w1+w2) (zipWith (++) (bpad h w1 i1) (bpad h w2 i2))
                        where h = max h1 h2

bottom (Pic h1 w1 i1) (Pic h2 w2 i2) 
                      = Pic h (w1+w2) (zipWith (++) (tpad h w1 i1) (tpad h w2 i2))
                        where h = max h1 h2

middle (Pic h1 w1 i1) (Pic h2 w2 i2) 
                      = Pic h (w1+w2) (zipWith (++) (mpad h w1 i1) (mpad h w2 i2))
                        where h = max h1 h2

tpad         :: Int -> Int -> [String] -> [String]
tpad h w ss   | l == h    = ss
              | l > h     = take h ss
              | otherwise = tpad h w (replicate w ' ' : ss)
              where l = length ss

bpad         :: Int -> Int -> [String] -> [String]
bpad h w ss   | l == h    = ss
              | l > h     = bpad h w (tail ss)
              | otherwise = bpad h w (ss ++ [replicate w ' '])
              where l = length ss

mpad         :: Int -> Int -> [String] -> [String]
mpad h w ss   | l == h    = ss
              | l-h == 1  = take h ss
              | l > h     = mpad h w (take (l-2) (tail ss))
              | otherwise = mpad h w ([replicate w ' '] ++ ss ++ [replicate w ' '])
              where l = length ss

hborder    :: Int -> Pic -> Pic
hborder n p = b `top` p `top` b
              where b = block (height p) n ' '

vborder    :: Int -> Pic -> Pic
vborder n p = b `left` p `left` b
              where b = block n (width p) ' '

vpad, hpad :: Int -> (Pic -> Pic -> Pic) -> Pic -> Pic
vpad n op p = vspace n `op` p
hpad n op p = hspace n `op` p


