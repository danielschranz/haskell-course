{-# LANGUAGE FlexibleInstances #-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)


--ex 1: implement lazy sequence of fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [0..]


--ex 2: again but no efficiently (to compute first n elements only O(n) additions)
fibs2 :: [Integer]
fibs2 = helper 0 1
        where helper :: Integer -> Integer -> [Integer]
              helper a b = a : helper b (a + b)


--ex 3:

--define a datatype of polymorphic streams
data Stream a = Stream a (Stream a)

-- write streamToList :: Stream a -> [a]
streamToList :: Stream a -> [a]
streamToList (Stream x rest) = x : streamToList rest

-- implement show for the first 20 elements of the stream for testing
instance Show a => Show (Stream a) where
  show x = show $ take 20 $ streamToList x
-- TODO can this be written in a wholemeal way?

-- so far I can't use stream yet because there is no way of instatiating one
-- Stream 1 $ Stream 2 $ Stream 3 (??) what to use as element for (??)
-- the helper functions in ex4 probably will help with that

-- ex 4:

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f $ f x

------------- that was a bit of a detour because I thought strict eval is enforced
--streamFromSeed f x = Stream x (helper f x) where
--  helper f x = streamFromSeed f (f x)                    -- that also :(


--streamFromSeed f x = Stream x (helper f x) where
--  helper f x = streamFromSeed f (f x)                      -- and that

--NOTE interesting - this modified streamRepeat also forces strict evaluation
--streamRepeat :: Floating a => a -> Stream a
--streamRepeat x = Stream x $ streamRepeat (x * x)

--NOTE weird, this one actually works
--streamRepeat :: Floating a => a -> Stream a
--streamRepeat x = Stream x $ streamRepeat (x ** 2)

--------------- end of detour

-- ex 5:
nats :: Stream Integer
nats = streamFromSeed succ 0

-- the ruler function (0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...)
-- where the nth element in the stream (assuming the first element corresponds to n = 1)
-- is the largest power of 2 which evenly divides n.

-- hint: define interleaveStreams which alternates the elements from two streams
--       can this be used to solve it without divisibility testing
ruler :: Stream Integer
--ruler = interleaveStreams unevenStream evenStream where
ruler = foldr1 interleaveStreams (map streamRepeat [0..]) where
  interleaveStreams :: Stream a -> Stream a -> Stream a
--  interleaveStreams (Stream x xs) (Stream y ys) = Stream x (Stream y (interleaveStreams xs ys))
  interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

-- http://codereview.stackexchange.com/questions/66700/implementing-function-that-maps-over-stream (I don't think I would have found that solution)

-- ex 6:

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger a = Stream a $ streamRepeat 0
  negate = streamMap (\x -> -x)
  (+) (Stream x xs) (Stream y ys) = Stream (x+y) (xs+ys)
  (*) (Stream xh xs) y@(Stream yh ys) = Stream (xh*yh) (a0TimesBRest + aRestTimesB) where
    a0TimesBRest = streamMap ((*) xh) ys
    aRestTimesB = xs * y

instance Fractional (Stream Integer) where
  (/) (Stream x xs) (Stream y ys) = Stream (x div y)
