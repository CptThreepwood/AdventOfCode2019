import Control.Monad.State
import qualified Data.Vector as V

type Location = Int
type Tape = V.Vector Int
data Machine = Machine {addr :: Location, tape :: Tape} deriving Show

readNext :: State Machine Int
readNext = state $ \s -> (readTape (tape s) (addr s), Machine ((addr s) + 1) (tape s))

skipTo :: Location -> State Machine ()
skipTo loc = state $ \s -> ((), Machine loc (tape s))

readTape :: Tape -> Location -> Int
readTape tape loc = tape V.! loc

readAt :: Location -> State Machine Int
readAt loc = state $ \s -> (readTape (tape s) loc, s)

updateTape :: Location -> Int -> Tape -> Tape
updateTape loc value tape = V.update tape $ V.fromList [(loc, value)]

writeTo :: Location -> Int -> State Machine ()
writeTo loc value = state $ \s -> ((), Machine (addr s) (updateTape loc value (tape s)))

parseOpcode :: Int -> [Int]
parseOpcode code
    | length codeS == 1 = [code]
    | otherwise = [read x | x <- codes]
    where codeS = show code
          x:y:xs = reverse codeS
          codes = [y,x]:[[z] | z <- xs]

getParam :: Int -> Int -> State Machine Int
getParam mode code
    | mode == 1 = return code
    | mode == 0 = readAt code
    | otherwise = return (-1) -- "Invalid Mode: " ++ show mode

-- evalOp :: State Machine (Int, Int, Int)
-- evalOp = do
--     opcode <- readNext
--     let (op, modes) = parseOpcode(opcode)
--     case op of
--         1 -> add modes
--         2 -> mult modes
--         3 -> input modes
--         4 -> output modes
--         -- 99 -> end
--         otherwise -> output []

rpad :: [Int] -> Int -> [Int]
rpad list len = take len $ list ++ repeat 0

add :: [Int] -> State Machine ()
add params = do
    xp <- readNext
    yp <- readNext
    saveParam <- readNext
    let padded = rpad params 3
    x <- getParam (padded !! 0) xp
    y <- getParam (padded !! 1) yp
    save <- getParam (padded !! 2) saveParam
    writeTo save (x + y)

mult :: [Int] -> State Machine ()
mult params = do
    xp <- readNext
    yp <- readNext
    saveParam <- readNext
    let padded = rpad params 3
    x <- getParam (padded !! 0) xp
    y <- getParam (padded !! 1) yp
    save <- getParam (padded !! 2) saveParam
    writeTo save (x * y)

-- input :: [Int] -> State Machine ()
-- input params = do
--     saveParam <- readNext
--     let padded = rpad params 1
--     save <- getParam (padded !! 0) saveParam
--     i <- getLine
--     writeTo save i

-- output :: [Int] -> IO ()
-- output params = do
--     outParam <- readNext
--     let padded = rpad params 1
--     out <- getParam (padded !! 0) outParam
--     print $ show out
