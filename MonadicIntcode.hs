import Control.Monad.State
import qualified Data.Vector as V

type Location = Int
type Tape = V.Vector Int
data Machine = Machine {addr :: Location, tape :: Tape} deriving Show

type Intcode = StateT Machine IO

io :: IO a -> StateT Machine IO a
io = liftIO

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

-- Parse Tape Instructions and Execute
type Param = Int
data Instruction = Unknown
    | Add Param Param
    | Mult Param Param
    | Read
    | Write Param
    | Halt
    deriving Show

parseOpcode :: Int -> Instruction
parseOpcode code
    | op == 1 = let [x, y] = take 2 params in Add x y
    | op == 2 = let [x, y] = take 2 params in Mult x y
    | op == 3 = Read
    | op == 4 = let [x] = take 1 params in Write x
    | op == 99 = Halt
    | otherwise = Unknown
    where x:y:xs = reverse $ "0" ++ show code
          op = read [y,x]
          params = [read [z] | z <- xs] ++ repeat 0

evalNext :: Intcode Int
evalNext = do
    opcode <- readNext
    let op = parseOpcode(opcode)
    case op of
        Add x y -> add x y >> return 0
        Mult x y -> mult x y >> return 0
        Read -> input >> return 0
        Write x -> output x >> return 0
        Halt -> return 1
        Unknown -> return (-1)

getParam :: Int -> Intcode Int
getParam mode = do
    x <- readNext
    case mode of
        1 -> return x
        0 -> readAt x
        otherwise -> return (-1) -- "Invalid Mode: " ++ show mode

add :: Int -> Int -> Intcode ()
add px py = do
    tot <- (+) <$> getParam px <*> getParam py
    save <- readNext
    writeTo save tot

mult :: Int -> Int -> Intcode ()
mult px py = do
    tot <- (*) <$> getParam px <*> getParam py
    save <- readNext
    writeTo save tot

input :: Intcode ()
input = do
    save <- readNext
    i <- io $ getLine
    writeTo save $ read i

output :: Int -> Intcode ()
output x = do
    out <- getParam x
    io $ print $ show out
    return ()
