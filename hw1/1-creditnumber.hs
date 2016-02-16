------------------- first implementation
-- toDigits :: Integer -> [Integer]
-- toDigits a
--    | a < 10 = [a]
--    | otherwise =  (toDigits (div a 10)) ++ [(mod a 10)]

------------------- implementation using a helper function
--toDigitsRev :: Integer -> [Integer]
--toDigitsRev 0 = []
--toDigitsRev a
--    | a < 10 = [a]
--    | otherwise =  (mod a 10) : toDigitsRev (div a 10)

--toDigits :: Integer -> [Integer]
--toDigits a = reverse (toDigitsRev a)

-------------------- a more concise version of toDigits mapping over a string
toDigits :: Integer -> [Integer]
toDigits a = map ((\x -> read x ::Integer) . (:[]))  (show a)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther s
    | (even . length) s = doubler s
    | otherwise = head s : doubler (tail s)
    where doubler :: [Integer] -> [Integer]
          doubler [] = []
          doubler (a:b:xs) = 2*a:b:(doubler xs)

-------------- first implementation
--sumDigits :: [Integer] -> Integer
--sumDigits [] = 0
--sumDigits (x:xs)
--    | x >9 = (mod x 10) + (div x 10) + sumDigits xs
--    | otherwise = x + sumDigits xs

-------------- using foldl
sumDigits :: [Integer] -> Integer
sumDigits s = foldl (\acc x -> acc + getVal x) 0 s
    where getVal = (\x -> if x<10 then x
                                  else (mod x 10) + (div x 10) )

validate :: Integer -> Bool
validate x = mod y 10 == 0
    where y = (sumDigits . doubleEveryOther . toDigits) x

-- nicer
--validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits)

