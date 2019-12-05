import System.Environment

hasDuplicate :: [Int] -> Bool
hasDuplicate (first:second:tail) = if (first == second) then True else hasDuplicate (second:tail)
hasDuplicate (_) = False

hasDecrease :: [Int] -> Bool
hasDecrease (first:second:tail) = if (first > second) then True else hasDecrease (second:tail)
hasDecrease (_) = False

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

main :: IO ()
main = do
   args <- getArgs
   let i = [197487 .. 673251]
   let digits = map toDigits i
   let noDecrease = filter (not . hasDecrease) digits
   let matches = filter hasDuplicate noDecrease
   putStrLn $ show $ length matches
