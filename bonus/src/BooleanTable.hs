{-# LANGUAGE FlexibleInstances #-}

module BooleanTable where

newtype BooleanTable = BooleanTable {getRows :: [([Bool], Bool)]}

class BooleanTableBuilder f where
    booleanTable :: f -> BooleanTable

instance BooleanTableBuilder Bool where
    booleanTable a = BooleanTable [([], a)]

instance (BooleanTableBuilder r) => BooleanTableBuilder (Bool -> r) where
    booleanTable f = BooleanTable $
        [(False : args, res) | (args, res) <- getRows . booleanTable $ f False] ++
        [(True  : args, res) | (args, res) <- getRows. booleanTable $ f True]

instance Show BooleanTable where
    show table = foldr1 (\a b -> a ++ "\n" ++ b) (map showRow $ getRows table)
      where
          showRow :: ([Bool], Bool) -> String
          showRow row = showArgs (fst row) ++ "= " ++ show (snd row)

          showArgs :: [Bool] -> String
          showArgs = foldr1 (++) . map prettyShow

          prettyShow :: Bool -> String
          prettyShow True  = "True  "
          prettyShow False = "False "
