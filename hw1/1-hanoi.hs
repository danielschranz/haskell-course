type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi x a b c = (hanoi (x-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (x-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, b)]
hanoi4 x a b c d = (hanoi4 y a c d b) ++ (hanoi z a d b) ++ (hanoi4 1 a b c d) ++ (hanoi z d b a) ++ (hanoi4 y c b a d)
    where y = div (x-1) 2
          z = (x-1) - y
