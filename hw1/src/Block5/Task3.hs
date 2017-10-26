{-# LANGUAGE FlexibleInstances #-}

module Block5.Task3 where
--
import           Block3.Task5
import           Block3.TreePrinters
import           Block4.Task1
import           Data.Semigroup      (Semigroup, (<>))

newtype MSTree t a = MSTree { getFTree :: t a }

instance (Ord a) => Monoid (MSTree (FTree Tree) a) where
  mempty = MSTree $ FTree Leaf

  mappend t1 t2 = MSTree $ foldr fInsert (getFTree t2) (getFTree t1)
    where
        fInsert a = FTree . insert a . getTree

instance (Ord a) => Semigroup (MSTree (FTree Tree) a) where
  (<>) = mappend

testTreeMappend :: IO ()
testTreeMappend = do
  x <- randomIntList 5 (-100) 100
  y <- randomIntList 5 (-100) 100
  putStrLn . verticalPrint . fromList $ x
  putStrLn . verticalPrint . fromList $ y
  putStrLn . verticalPrint . getTree. getFTree $ MSTree (FTree (fromList x)) <> MSTree (FTree (fromList y))
