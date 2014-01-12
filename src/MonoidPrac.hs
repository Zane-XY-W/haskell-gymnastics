module MonoidPrac where

import Data.Foldable 
import Data.Monoid

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

instance  Foldable Tree  where
  foldMap _ Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x `mappend`
                           foldMap f r

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

fromDiffList :: DiffList a -> [a]
fromDiffList f = getDiffList f []

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList( xs ++)

instance Monoid (DiffList a) where
  mempty = DiffList(\xs -> xs ++ [])
  (DiffList f) `mappend` (DiffList g) = DiffList(\xs -> f (g xs)) 

treeData :: Tree Integer
treeData = Node 5  
  (Node 3  
   (Node 1 Empty Empty)  
   (Node 6 Empty Empty)  
  )  
  (Node 9  
   (Node 8 Empty Empty)  
   (Node 10 Empty Empty)  
  )                              