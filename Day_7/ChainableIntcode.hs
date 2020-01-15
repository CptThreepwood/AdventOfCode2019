import Control.Monad.State
import qualified Data.Vector as V
import Data.List.Split
import Data.List

type Location = Int
type Tape = V.Vector Int
data Machine = Machine {
    addr :: Location,
    inbuf :: Tape,
    outbuf :: Tape,
    tape :: Tape
} deriving Show

-- Include IO in our Interpreter
type Intcode = StateT Machine IO

io :: IO a -> StateT Machine IO a
io = liftIO

-- Low level tape managemet
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

addOutput :: Int -> Intcode ()
addOutput value = modify $ \s -> s {outbuf = V.snoc (outbuf s) value}

popInput :: Intcode Int
popInput = do
    inputTape <- gets inbuf
    case V.null inputTape of
        True -> do io $ read <$> getLine
        False -> state $ \s -> (V.head inputTape, s {inbuf = V.drop 1 inputTape})

-- Parse Tape Instructions and Execute
type Param = Int
data Instruction = Unknown
    | Add Param Param
    | Mult Param Param
    | Read
    | Write Param
    | Jump Bool Param Param
    | Lt Param Param
    | Eq Param Param
    | Halt
    deriving Show

parseOpcode :: Int -> Instruction
parseOpcode code
    | op == 1 = let [x, y] = take 2 params in Add x y
    | op == 2 = let [x, y] = take 2 params in Mult x y
    | op == 3 = Read
    | op == 4 = let [x] = take 1 params in Write x
    | op == 5 = let [x, y] = take 2 params in Jump True x y
    | op == 6 = let [x, y] = take 2 params in Jump False x y
    | op == 7 = let [x, y] = take 2 params in Lt x y
    | op == 8 = let [x, y] = take 2 params in Eq x y
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
        Jump True x y -> jump True x y >> return 0
        Jump False x y -> jump False x y >> return 0
        Lt x y -> comp (<) x y >> return 0
        Eq x y -> comp (==) x y >> return 0
        Halt -> return 1
        Unknown -> return (-1)

getParam :: Param -> Intcode Int
getParam mode = do
    x <- readNext
    case mode of
        1 -> return x
        0 -> readAt x
        otherwise -> return (-1) -- "Invalid Mode: " ++ show mode

add :: Param -> Param -> Intcode ()
add px py = do
    tot <- (+) <$> getParam px <*> getParam py
    save <- readNext
    writeTo save tot

mult :: Param -> Param -> Intcode ()
mult px py = do
    tot <- (*) <$> getParam px <*> getParam py
    save <- readNext
    writeTo save tot

input :: Intcode ()
input = do
    save <- readNext
    i <- popInput
    writeTo save i

output :: Param -> Intcode ()
output x = do
    out <- getParam x
    addOutput out
    -- io $ print $ show out
    return ()

jump :: Bool -> Param -> Param -> Intcode ()
jump t x y = do
    condition <- getParam x
    loc <- getParam y
    if (t && condition /= 0) || ((not t) && condition == 0) then skipTo loc else return ()

comp :: (Int -> Int -> Bool) -> Param -> Param -> Intcode ()
comp op x y = do
    left <- getParam x
    right <- getParam y
    save <- readNext
    if op left right then writeTo save 1 else writeTo save 0

-- Interpreter entrypoint
interpret :: Intcode ()
interpret = do
    status <- evalNext
    if status == 0 then interpret else return ()

-- Run Code

string2Tape :: String -> Tape
string2Tape content = V.fromList . map read . splitOn "," $ content

type Settings = [Int]

runAmp :: Tape -> Tape -> IO Int
runAmp program i = do
    a <- execStateT interpret $ Machine 0 i (V.fromList []) program
    return (V.head $ outbuf a)

runAmps :: Tape -> Settings -> IO Int
runAmps program settings = foldM (\acc x -> runAmp program (V.fromList [x, acc])) 0 settings

main = do
    -- Tests
    -- let program = string2Tape "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    -- thrusts <- mapM (runAmps program) $ Data.List.permutations [0..4]
    -- print $ (maximum thrusts) == 43210
    -- let program = string2Tape "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    -- thrusts <- mapM (runAmps program) $ Data.List.permutations [0..4]
    -- print $ (maximum thrusts) == 54321
    -- let program = string2Tape "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    -- thrusts <- mapM (runAmps program) $ Data.List.permutations [0..4]
    -- print $ (maximum thrusts) == 65210

    -- Part 1
    program <- string2Tape <$> readFile "input_7.txt"
    thrusts <- mapM (runAmps program) $ Data.List.permutations [0..4]
    print $ (maximum thrusts)
