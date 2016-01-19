3.
sommeDeXaY a b = sum [a .. b]

4.
somme []     = 0
somme (x:xs) = x + (somme xs)

5.
dernier (x:[]) = x
dernier (x:xs) = dernier xs
dernier []     = error "liste vide"

6.
premiers (_:[]) = []
premiers (x:xs) = x : (premiers xs)
premiers []     = error "liste vide"

7.
(!!!) (x:_) 0  = x
(!!!) (x:xs) i = xs !!! (i - 1)
(!!!) [] _     = error "liste vide"

(+++) [] []     = []
(+++) [] (y:[]) = [y]
(+++) [] (y:ys) = y : [] +++ ys
(+++) (x:xs) ys = x : xs +++ ys

concat_l (xs:[])  = xs
concat_l (xs:xss) = xs +++ concat_l xss

my_map fct (x:[]) = fct x : []
my_map fct (x:xs) = (fct x) : (my_map fct xs)

8.
my_length l = somme (map (\_ -> 1) l)

9.
my_function f x n = take n (iterate f x)

10.
loop n = my_function (+ 1) 1 n