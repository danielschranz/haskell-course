mymap :: (a -> b) -> [a] -> [b]
mymap fn [] = []
mymap fn (x:xs) = fn x : mymap fn xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter fn [] = []
myfilter fn (x:xs)
    | (fn x) == True = x : myfilter fn xs
    | otherwise = myfilter fn xs

myfold :: (b -> a -> b) -> b -> [a] -> b
myfold _ acc [] = acc
myfold fn acc (x:xs) = myfold fn (fn acc x) xs