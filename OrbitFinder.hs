import qualified Data.Text as T
import qualified Data.Map.Strict as M

type OrbitGraph = M.Map T.Text [T.Text]

parseOrbit :: T.Text -> (T.Text, [T.Text])
parseOrbit orbit = let [x, y] = take 2 $ T.splitOn (T.pack ")") orbit in (x, [y])

createOrbitGraph :: [T.Text] -> OrbitGraph
createOrbitGraph = (M.fromListWith (++)) . (map parseOrbit)

-- There's probably ways around this involving combining [] and Maybe applicatives
lookupChildren :: OrbitGraph -> T.Text -> [T.Text]
lookupChildren graph key = case result of
    Nothing -> []
    Just x -> x
    where result = M.lookup key graph 

sumOrbits :: OrbitGraph -> Int -> T.Text -> Int
sumOrbits graph depth node
    | children == [] = depth
    | otherwise = depth + (sum $ map (sumOrbits graph nextDepth) children)
    where children = lookupChildren graph node
          nextDepth = depth + 1

main = do
    orbitGraph <- createOrbitGraph <$> map T.pack <$> lines <$> readFile "input_6.txt"
    print $ sumOrbits orbitGraph 0 (T.pack "COM")
