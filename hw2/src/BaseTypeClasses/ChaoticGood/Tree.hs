{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Tree where

import           Prelude (Applicative, Foldable, Functor, Traversable, fmap,
                          foldr, pure, sequenceA, traverse, ($), (<$>), (<*>))

data Tree a = Leaf | Node (Tree a) (Tree a) a

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap f (Node l r a) = Node (fmap f l) (fmap f r) (f a)

instance Applicative Tree where
    pure a = Node (pure a) (pure a) a

    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node l1 r1 f) <*> (Node l2 r2 a) = Node (l1 <*> l2) (r1 <*> r2) (f a)

instance Foldable Tree where
    foldr _ z Leaf         = z
    foldr f z (Node l r a) = foldr f (f a $ foldr f z r) l

instance Traversable Tree where
    sequenceA Leaf         = pure Leaf
    sequenceA (Node l r f) = Node <$> sequenceA l <*> sequenceA r <*> f

    traverse _ Leaf         = pure Leaf
    traverse f (Node l r a) = Node <$> traverse f l <*> traverse f r <*> f a

-- PROOFS
{- Functor1: fmap id === id
    Base:
    fmap id (Leaf) === Leaf                                                 -- definition of fmap

    Inductive step:
    Inductive assumption:
    ] fmap id l === l AND fmap id r === r
    fmap id (Node l r a) === Node (fmap id l) (fmap id r) (id a)
                         === Node l r (id a)                               -- inductive assumption
                         === Node l r a                                    -- definition of id
-}

{- Functor2: fmap f . fmap g === fmap (f . g)
    Base:
    fmap f (fmap g Leaf) === Leaf                                           -- definition of fmap
    fmap (f . g) Leaf    === Leaf                                           -- definition of fmap

    Inductive step:
    Inductive assumption:
    ] fmap f . fmap g $ l === fmap (f . g) l AND fmap f . fmap g $ r === fmap (f . g) r
    (fmap f . fmap g) (Node l r a) === fmap f (fmap g (Node l r a))             -- assumption about (.)
                                   === fmap f
                                        (Node (fmap g l)
                                        (fmap g r)
                                        (g a))                                  -- definition of fmap
                                   === Node (fmap f (fmap g l))
                                            (fmap f (fmap g r))
                                            (f (g a))                           -- definition of fmap
                                   === Node (fmap f . fmap g $ l)
                                            (fmap f . fmap g $ r)
                                            (f . g $ a)                         -- assumption about (.)
                                   === Node (fmap (f . g) l)
                                            (fmap (f . g) r)
                                            (f . g $ a)                         -- inductive assumption
                                   === fmap (f . g) $ Node l r a                -- definition of fmap
-}

{- Applicative1: pure id <*> v === v
    Base:
    pure id <*> Leaf === Leaf                                                   -- definition of <*>

   Inductive step:
   Inductive assumption:
   ] pure id <*> l === l AND pure id <*> r === r
   pure id <*> Node l r a   === Node (pure id) (pure id) id <*> Node l r a
                            === Node (pure id <*> l) (pure id <*> r) (id a)     -- definition of <*>
                            === Node l r a                                      -- inductive assumption
-}

-- Using transfinite induction where M - nodes set, ∀x ∈ M, y ∈ M:
-- x < y <=> x subtree of y or x in left branch and y in right branch of LCA
-- M - well-ordered set, so we can use transfinite induction:
-- Proposition A(x) holds for every element x of a well-ordered set M if it is established
-- that for each z ∈ E, the truth of A(y) for all y < z implies the truth of A(z)

{- Applicative3: pure f <*> pure x = pure (f x)
    pure f <*> pure x === Node (pure f) (pure f) f <*> Node (pure x) (pure x) x -- definition of pure
                      === Node (pure f <*> pure x) (pure f <*> pure x) (f x)    -- definition of <*>

    pure (f x)        === Node (pure (f x)) (pure (f x)) (f x)                  -- definition of pure
-}

{- Applicative4: u <*> pure y = pure ($ y) <*> u
    Base:
    pure ($ y) <*> Leaf === Leaf                                                -- definition of <*>
    Leaf <*> pure y     === Leaf                                                -- definition of <*>

    Inductive step:
    Inductive assumption:
    ] pure ($y) <*> l === l <*> pure y AND pure ($y) <*> r === r <*> pure y
   pure ($y) <*> Node l r f === Node (pure ($y)) (pure ($y)) ($y) <*> Node l r f    -- definition of pure
                            === Node (pure ($y) <*> l) (pure ($y) <*> r) (f y)      -- definition if <*>
                            === Node (l <*> pure y) (r <*> pure y) (f y)            -- inductive assumption

    Node l r f <*> pure y   === Node l r f <*> Node (pure y) (pure y) (f y)         -- definition of pure
                            === Node (l <*> pure y) (r <*> pure y) (f y)            -- definition of <*>
-}
