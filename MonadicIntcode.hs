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

getParam :: Int -> Intcode Int
getParam mode = do
    x <- readNext
    case mode of
        1 -> return x
        0 -> readAt x
        otherwise -> return (-1) -- "Invalid Mode: " ++ show mode

-- evalOp :: Intcode ()
-- evalOp = do
--     opcode <- readNext
--     let op = parseOpcode(opcode)
--     case op of
--         Add x y z -> add x y z
--         Mult x y z -> mult x y z
--         Read x -> input x
--         Write x -> output x
--         -- 99 -> end
--         otherwise -> output []

add :: Int -> Int -> Int -> Intcode ()
add px py pz = do
    tot <- (+) <$> getParam px <*> getParam py
    save <- getParam pz
    writeTo save tot

mult :: Int -> Int -> Int -> Intcode ()
mult px py pz = do
    tot <- (*) <$> getParam px <*> getParam py
    save <- getParam pz
    writeTo save tot

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
