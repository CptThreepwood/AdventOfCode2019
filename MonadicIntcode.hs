import Control.Monad.State
import qualified Data.Vector as V

type Location = Int
type Tape = V.Vector Int
data Machine = Machine {addr :: Location, tape :: Tape} deriving Show

readTape :: State Machine Int
readTape = state $ \s -> ((tape s) V.! (addr s), Machine ((addr s) + 1) (tape s))

skipTo :: Int -> State Machine ()
skipTo loc = state $ \s -> ((), Machine loc (tape s))

updateTape :: Int -> Int -> Tape -> Tape
updateTape loc value tape = V.update tape $ V.fromList [(loc, value)]

write :: Int -> Int -> State Machine ()
write loc value = state $ \s -> ((), Machine (addr s) (updateTape loc value (tape s)))

-- apply :: (Int -> Int -> Int) -> Int -> State -> State
-- apply f addr state  =
--     let pos1 = state V.! (addr + 1)
--         pos2 = state V.! (addr + 2)
--         pos3 = state V.! (addr + 3)
--         total = f (state V.! pos1) (state V.! pos2)
--     in V.update state $ V.fromList [(pos3, total)]

add :: State Machine ()
add = do
    x <- readTape
    y <- readTape
    z <- readTape
    write z (x + y)

-- process :: State Machine
-- process = do
--     code <- readTape
--     case code of
--         1 -> 
--         99 -> return

        
-- process = State $ \s -> 
--     | code == 1 = process next (apply (+) addr state) 
--     | code == 2 = process next (apply (*) addr state)
--     | code == 99 = state
--     | otherwise = error $ "Unknown OpCode: " ++ show code
--     where code = state V.! addr
--             next = (addr + 4)