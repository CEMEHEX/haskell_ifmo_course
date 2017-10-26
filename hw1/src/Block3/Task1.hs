module Block3.Task1 where

data DaysOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Bounded, Enum, Eq, Show)

nextDay :: DaysOfWeek -> DaysOfWeek
nextDay x | x == maxBound = minBound
          | otherwise = succ x

daysInWeek :: Int
daysInWeek = fromEnum (maxBound :: DaysOfWeek) - fromEnum (minBound :: DaysOfWeek) + 1

afterDays :: DaysOfWeek -> Int -> DaysOfWeek
afterDays curDay dayCnt = toEnum $ mod (mod dayCnt daysInWeek - fromEnum curDay) daysInWeek

isWeekend :: DaysOfWeek -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

daysToParty :: DaysOfWeek -> Int
daysToParty curDay = mod (fromEnum Fri - fromEnum curDay) daysInWeek
