getModuleFuel :: Int -> Int
getModuleFuel mod = mod `quot` 3 - 2

totalFuel :: Int -> Int
totalFuel mod
    | fuel > 0 = fuel + totalFuel fuel
    | otherwise = 0
    where fuel = getModuleFuel mod

main = do
    content <- readFile "input_1.txt"
    -- Answer 1
    print $ sum
          . map (getModuleFuel . read) 
          . lines
          $ content
    -- Answer 2
    print $ sum
          . map (totalFuel . read) 
          . lines
          $ content
