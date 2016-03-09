module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons employee@(Emp {empFun=empFun} ) (GL employees fun) = GL (employee:employees) (empFun+fun)

-- NOTE this Monoid implementation must be wrong because the instructions mention I would have to create an orphan instance
instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL a funA) (GL b funB) = GL (a ++ b) (funA + funB)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ funA) b@(GL _ funB) = if funA > funB then a else b


-- exercise 2

-- simple approach which doesn't work (read http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf for an explanation why)
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss@(Emp {empFun=bossFun}) subDivisions = if subFun > bossFun then subDivGuestList else (GL [boss] bossFun) where
  subDivGuestList@(GL _ subFun) = mconcat subDivisions

treeFold :: (a -> b) -> (b -> b -> b) -> Tree a -> b
treeFold f g (Node {rootLabel = root, subForest = forest}) = foldl1 g ((f root):(map (treeFold f g) forest))


-- exercise 3

-- TODO refactoring needed to make this more readable I think (I temporarily moved the helper functions out of the where clause to have access to it from the outside)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp {empFun=bossFun}) departments = (glCons boss exclSubBossesSum, inclSubBossesSum) where
  (inclSubBossesSum, exclSubBossesSum) = foldl1 multiMappend departments

multiMappend (aInclSubBosses, aExclSubBosses) (bInclSubBosses, bExclSubBosses) = (mappend (moreFun aExclSubBosses aInclSubBosses) (moreFun bExclSubBosses bInclSubBosses), mappend aExclSubBosses bExclSubBosses)


-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun x = moreFun listA listB where
  (listA, listB) = helper x

helper (Node x []) = (glCons x mempty, mempty)
helper (Node x y) = nextLevel x (map helper y)

--exercise 5

formatList :: GuestList -> String
formatList (GL list fun) = "Total fun: " ++ (show fun) ++ "\n" ++ (unlines . (map show)) (sortBy (\(Emp namea _) (Emp nameb _) -> compare namea  nameb)  list)

main :: IO ()
main = fmap (formatList . maxFun . read) (readFile "company.txt") >>= putStrLn


--------- test
myCompany = Node (Emp "A" 19)
            [ Node (Emp "B" 17) [
                  Node (Emp "E" 4) [
                      Node (Emp "G" 2) [],
                      Node (Emp "H" 6) []
                      ],
                  Node (Emp "F" 12) []
                  ],
              Node (Emp "C" 1) [],
              Node (Emp "D" 2) []
            ]
-- maxFun myComoany should be A, F, G, H and sum up to 39
