import qualified Data.Text as T
import qualified Data.Tuple as Tup
import qualified Data.Map.Strict as M

type Label = T.Text
type Orbit = (Label, Label)
type OrbitGraph = M.Map Label [Label]

parseOrbit :: Label -> Orbit
parseOrbit orbit = let [x, y] = take 2 $ T.splitOn (T.pack ")") orbit in (x, y)

createOrbitGraph :: [Orbit] -> OrbitGraph
createOrbitGraph orbits = M.fromListWith (++) $ map (\(x,y) -> (x,[y])) $ orbits ++ map Tup.swap orbits

-- There's probably ways around this involving combining [] and Maybe applicatives
lookupChildren :: OrbitGraph -> Label -> Label -> [Label]
lookupChildren graph parent key = case result of
    Nothing -> []
    Just x -> filter (\y -> y /= parent) x
    where result = M.lookup key graph 

sumOrbits :: OrbitGraph -> Int -> Label -> Label -> Int
sumOrbits graph depth parent node
    | children == [] = depth
    | otherwise = depth + (sum $ map (sumOrbits graph nextDepth node) children)
    where children = lookupChildren graph parent node
          nextDepth = depth + 1

-- Convenience Function for the total orbits from COM
orbitsFromCOM :: OrbitGraph -> Int
orbitsFromCOM graph = sumOrbits graph 0 (T.pack "") (T.pack "COM")

pathLength :: OrbitGraph -> Label -> Int -> Label -> Label -> Int
pathLength tree key depth parent node
    | found == True = depth
    | children == [] = 0
    | otherwise = sum $ map (pathLength tree key nextDepth node) children
    where children = lookupChildren tree parent node
          found = key `elem` children
          nextDepth = depth + 1

-- Convenience function for path length between start and key
search :: OrbitGraph -> Label -> Label -> Int
search graph start end = pathLength graph end 1 (T.pack "") start

main = do
    orbitGraph <- createOrbitGraph <$> map (parseOrbit . T.pack) <$> lines <$> readFile "input_6.txt"
    print $ orbitsFromCOM orbitGraph

    -- Assuming these exist and there is only one answer
    let Just [myOrbit] = M.lookup (T.pack "YOU") orbitGraph
    let Just [santaOrbit] = M.lookup (T.pack "SAN") orbitGraph
    print $ search orbitGraph myOrbit santaOrbit
