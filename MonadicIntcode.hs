import Control.Monad.State
import qualified Data.Vector as V

type Location = Int
type Tape = V.Vector Int
data Machine = Machine {addr :: Location, tape :: Tape} deriving Show

type Intcode = StateT Machine IO

readNext :: Intcode Int
readNext = do
    s <- get
    let current = readTape (addr s) (tape s)
    put $ s {addr = addr s + 1}
    return current

skipTo :: Location -> Intcode ()
skipTo loc = modify $ \s -> s {addr = loc}

readTape :: Location -> Tape -> Int
readTape = flip (V.!)

readAt :: Location -> Intcode Int
readAt loc = gets $ readTape loc . tape

updateTape :: Location -> Int -> Tape -> Tape
updateTape loc value tape = V.update tape $ V.fromList [(loc, value)]

writeTo :: Location -> Int -> Intcode ()
writeTo loc value = modify $ \s -> s {tape = updateTape loc value (tape s)}

type Param = Int

data Instruction = Unknown
    | Add Param Param Param
    | Mult Param Param Param
    | Read Param
    | Write Param
    | Halt
    deriving Show

parseOpcode :: Int -> Instruction
parseOpcode code
    | op == 1 = let [x, y, z] = take 3 params in Add x y z
    | op == 2 = let [x, y, z] = take 3 params in Mult x y z
    | op == 3 = let [x] = take 1 params in Read x
    | op == 4 = let [x] = take 1 params in Write x
    | op == 99 = Halt
    | otherwise = Unknown
    where x:y:xs = reverse $ "0" ++ show code
          op = read [y,x]
          params = [read [z] | z <- xs] ++ repeat 0

getParam :: Int -> Int -> Intcode Int
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

add :: [Int] -> Intcode ()
add params = do
    xp <- readNext
    yp <- readNext
    saveParam <- readNext
    let padded = rpad params 3
    x <- getParam (padded !! 0) xp
    y <- getParam (padded !! 1) yp
    save <- getParam (padded !! 2) saveParam
    writeTo save (x + y)

mult :: [Int] -> Intcode ()
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
