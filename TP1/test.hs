sommeDeXaY a b = sum [a .. b]

somme []     = 0
somme (x:xs) = x + (somme xs)

dernier (x:[]) = x
dernier (x:xs) = dernier xs
dernier []     = error "liste vide"

premiers (_:[]) = []
premiers (x:xs) = x : (premiers xs)
premiers []     = error "liste vide"

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
