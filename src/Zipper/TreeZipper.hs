module TreeZipper where

-- | a tree is either a fork or a leaf
data Tree a = Fork (Tree a) (Tree a) | Leaf a

-- | a context has a hole at either left or right, and a context or a path
-- to root
data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)

-- | a Loc forms a whole tree, subtree + context
type Loc a = (Tree a, Cxt a)

-- | go to left of a fork, Fork also represents the focus
-- go left result in a hole in the left, a context has a hole in the left
-- is L c r
left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)

-- | go to right of a fork
right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)

-- | takes a tree, and returns a Loc with focus at the top
top :: Tree a -> Loc a
top t = (t, Top)

-- | up means moves focus to a upper level,
-- if the hole has a right sibling r, then after moving the hole up,
-- the subtree attached to the hole becomes the siblings of r,
-- that's why Fork t r make sense here.
-- also, the current context is L c r, and c means the context of L, or
-- think of it as parent context
up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)
