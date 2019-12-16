-- Check that tuple elements are equal
areEqual :: (Eq a) => (a, a) -> Bool
areEqual (x, y) = x == y

-- Check that the second element is not less than the first
notDecreasing :: (Ord a) => (a, a) -> Bool
notDecreasing (x, y) = x <= y

-- Potentially loops through the list twice, but it's cleaner looking
meetsCriteria :: (Ord a, Eq a) => [(a, a)] -> Bool
meetsCriteria list = (or $ map areEqual list) && (and $ map notDecreasing list)

-- Create tuples out of pairs of elements in list
tuplise :: [a] -> [(a, a)]
tuplise list = zip (init list) (tail list)

validCodes :: [Int] -> [Int]
validCodes codes = filter (meetsCriteria . tuplise . show) codes

main = do
    -- Test 1
    -- print $ (meetsCriteria . tuplise $ "122345") == True
    -- print $ (meetsCriteria . tuplise $ "111123") == True
    -- print $ (meetsCriteria . tuplise $ "135679") == False
    -- print $ (meetsCriteria . tuplise $ "111111") == True
    -- print $ (meetsCriteria . tuplise $ "223450") == False
    -- print $ (meetsCriteria . tuplise $ "123789") == False
    -- Answer 1
    print $ length . validCodes $ [168630..718098]