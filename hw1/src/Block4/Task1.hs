{-# LANGUAGE FlexibleInstances #-}

module Block4.Task1 where

import           Block3.TreePrinters

newtype FTree t a = FTree { getTree :: t a }

instance Foldable (FTree Tree) where
  foldMap _ (FTree Leaf)         = mempty
  foldMap f (FTree (Node a l r)) = foldMap f (FTree l) `mappend` f a `mappend` foldMap f (FTree r)

  foldr _ z (FTree Leaf)         = z
  foldr f z (FTree (Node a l r)) = foldr f (f a (foldr f z (FTree r))) (FTree l)
