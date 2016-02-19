{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


-- exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

-- implement (+++):
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (mappend (tag x) (tag y)) x y



-- exercise 2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ x y) = jlToList x ++ jlToList y

-- implement indexJ:
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ y) = Just y
--indexJ _ (Single _ _) = Nothing    -- only necessary if Single has wrong size (not 1)
indexJ i _ | i<0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Append s _ _) | i>=(getSize (size s)) = Nothing

indexJ 0 (Append _ x _) = indexJ 0 x
indexJ i (Append _ (Single _ _) y) = indexJ (i-1) y
indexJ i (Append _ x@(Append sx _ _) y)
  | i< getSize (size sx) = indexJ i x
  | otherwise = indexJ (i-(getSize (size sx))) y

-- implement dropJ
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i x | i<1 = x
dropJ _ (Single _ _) = Empty

dropJ i (Append _ (Single _ _) y) = dropJ (i-1) y
dropJ i (Append _ x@(Append s _ _) y) = dropJ i x +++ dropJ (i-(getSize (size s))) y


--implement takeJ
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i<1 = Empty
takeJ _ x@(Single _ _) = x

takeJ i (Append _ x@(Single _ _) y) = x +++ takeJ (i-1) y
takeJ i (Append _ x@(Append s _ _) y) = takeJ i x +++ takeJ (i - (getSize (size s))) y

-- test with
--let x=(Append (Size 4) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')) (Append (Size 2) (Single (Size 1) 'c') (Single (Size 1) 'd')))


-- exercise 3 (see also Scrabble module)

---- scoreLine "yay " +++ scoreLine "haskell!"

-- Append (Score 23)
-- (Single (Score 9) "yay ")
-- (Single (Score 14) "haskell!")

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- exercise 4

instance Buffer (JoinList (Score, Size) String) where
  -- | Convert a buffer to a String.
  toString Empty = ""
  toString (Single _ x) = x
  toString (Append _ x y) = toString x ++ toString y

  -- | Create a buffer from a String.
  fromString = foldr1 (+++) . map (\x -> Single (scoreString x, Size (length x)) x) . lines

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line _ Empty = Nothing
  line i _ | i<0 = Nothing
  line 0 (Single _ x) = Just x
  line _ (Single _ _) = Nothing
  line 0 (Append _ x _) = line 0 x
  line i (Append _ _ y) = line (i-1) y

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine _ _ Empty = Empty
  replaceLine i _ x | i<0 = x
  replaceLine 0 s (Single _ _) = fromString s
  replaceLine _ _ x@(Single _ _) = x
  replaceLine 0 s (Append _ x y) = fromString s +++ y
  replaceLine i s (Append _ x y) = x +++ replaceLine (i-1) s y

  -- | Compute the number of lines in the buffer.
  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append _ x y) = numLines x + numLines y

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value Empty = 0
  value (Single ((Score s),_) _) = s
  value (Append ((Score s),_) _ _) = s


buffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String

main = runEditor editor $ buffer
