module Block3.Task3 where

data Vector a = Vec2D a a
              | Vec3D a a a
              deriving (Eq)

toList :: (Num a) => Vector a -> [a]
toList (Vec2D x y)   = [x, y, 0]
toList (Vec3D x y z) = [x, y, z]

fromList :: (Num a, Eq a) => [a] -> Vector a
fromList [x, y]    = Vec2D x y
fromList [x, y, 0] = Vec2D x y
fromList [x, y, z] = Vec3D x y z
fromList _         = error "Wrong dimension"

len :: (Floating a) => Vector a -> a
len = sqrt . sum . map (^(2 :: Integer)) . toList

vecSum :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
vecSum v1 v2 = fromList $ zipWith (+) (toList v1) (toList v2)

scalarMul :: (Num a, Eq a) => Vector a -> Vector a -> a
scalarMul v1 v2 = sum $ zipWith (*) (toList v1) (toList v2)

dist :: (Floating a) => Vector a -> Vector a -> a
dist v1 v2 = sqrt . sum . map (^(2 :: Integer)) $ zipWith (-) (toList v1) (toList v2)

vecMul :: (Num a) => Vector a -> Vector a -> Vector a
vecMul v1 v2 = Vec3D (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)
  where
      [x1, y1, z1] = toList v1
      [x2, y2, z2] = toList v2

instance (Show a) => Show (Vector a) where
    show (Vec3D x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
    show (Vec2D x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
