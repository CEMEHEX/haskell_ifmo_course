module Block2.Task1 where

remove :: Int -> [a] -> (Maybe a, [a])
remove n list
             | n >= 0  = (Just $ list !! n, take n list ++ drop (n + 1) list)
             | otherwise = (Nothing, list)


-- remove n list = helper 0 list Nothing []
--   where
--     helper :: Int -> [a] -> Maybe a -> [a] -> (Maybe a, [a])
--     helper _ [] element resList = (element, reverse resList)
--     helper i (x:xs) element resList = helper (i + 1) xs newElement newResList
--       where
--         (newElement, newResList) = if n == i then (Just x, resList) else (element, x:resList)
