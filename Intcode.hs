import qualified Data.Vector as V
import Data.List.Split

apply :: (Int -> Int -> Int) -> Int -> V.Vector Int -> V.Vector Int
apply f addr state  =
    let pos1 = state V.! (addr + 1)
        pos2 = state V.! (addr + 2)
        pos3 = state V.! (addr + 3)
        total = f (state V.! pos1) (state V.! pos2)
    in V.update state $ V.fromList [(pos3, total)]

process :: Int -> V.Vector Int -> V.Vector Int
process addr state
    | code == 1 = process next (apply (+) addr state) 
    | code == 2 = process next (apply (*) addr state)
    | code == 99 = state
    | otherwise = error $ "Unknown OpCode: " ++ show code
    where code = state V.! addr
          next = (addr + 4)

start :: V.Vector Int -> Int
start state = (process 0 state) V.! 0

compile :: V.Vector Int -> ((Int, Int) -> Int)
compile program = start . (V.update program) . vectorizeArgs

vectorizeArgs :: (Int, Int) -> V.Vector (Int, Int)
vectorizeArgs args = 
    let (x, y) = args
    in V.fromList [(1, x), (2, y)]

string2State :: String -> V.Vector Int
string2State content = V.fromList . map read . splitOn "," $ content

main = do
    -- Tests
    -- print $ (process 0 $ V.fromList [1, 0, 0, 0, 99]) == V.fromList [2,0,0,0,99]
    -- print $ (process 0 $ V.fromList [2, 3, 0, 3, 99]) == V.fromList [2,3,0,6,99]
    -- print $ (process 0 $ V.fromList [2, 4, 4, 5, 99, 0]) == V.fromList [2,4,4,5,99,9801]
    -- print $ (process 0 $ V.fromList [1, 1, 1, 4, 99, 5, 6, 0, 99]) == V.fromList [30,1,1,4,2,5,6,0,99]
    --
    content <- readFile "input_2.txt"
    -- Edit the initial state from the input log
    let program = compile $ string2State content
    -- Answer 1
    print $ program (12, 2)
    -- Answer 2
    let params = head [(x, y) | x <- [0..99], y <- [0..99], program (x, y) == 19690720]
    print $ 100 * (fst params) + (snd params)