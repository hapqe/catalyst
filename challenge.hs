import Data.Function (on)
import Data.List (groupBy, sortBy, transpose)

distance = length . filter (== 'F')

dirs :: [[Int]]
dirs = [[0, 1], [1, 0], [0, -1], [-1, 0]]

perform 'F' p i = (zipWith (+) p $ cycle dirs !! i, i)
perform 'L' p i = (p, i - 1)
perform 'R' p i = (p, i + 1)

stretch [] = []
stretch (cmd : n : rest) = replicate (read n :: Int) cmd ++ stretch rest

actions = concat . stretch . tail . words

performAll _ p [] = (p, [])
performAll prevI positions@(prevPos : _) (action : as) =
  let (pos, i) = perform action prevPos prevI
   in performAll i (pos : positions) as

square =
  product
    . map ((-) . minimum <*> maximum)
    . transpose
    . fst
    . performAll 400 [[0, 0]]

solution = putStrLn . unwords . map show . ((:) . distance <*> return . square) . actions

rotate = drop <> take

velocities :: [[Int]] -> [[Int]]
velocities = (.) id (<*> rotate 1) $ zipWith $ zipWith (-)

verticals = map (!! 0) . sortBy (compare `on` (!! 1)) . map offset . filter ((/= 0) . yv) . (zip <*> velocities)
  where
    yv = (!! 1) . snd
    offset ([a, b], [_, 1]) = [a, b - 1]
    offset ([a, b], [_, _]) = [a, b]

area = compute . verticals
  where
    compute [] = 0
    compute (a : b : bs) = max a b - min a b + compute bs

inputs =
  [ "1 FFFR 4",
    "9 F 6 R 1 F 4 RFF 2 LFF 1 LFFFR 1 F 2 R 1 F 5",
    "14 L 1 FR 1 FFFFFL 1 FFFFL 1 F 12 L 1 F 12 L 1 F 12 L 1 FFFFL 1 FFFFFFFFR 1 FFFR 1 FFFL 1",
    "32 FFRFLFLFFRFRFLFF 3 R 1 FFLFRFRFLFFF 3 R 1 FFFFFF 3 L 1 FFFRFLFLFRFF 2 R 1 FFFRFLFLFRFF 3 R 1 FFFFFF 1 L 1 FFRFLFLFFRFRFLFF 3 R 1 FFLFRFRFFLFLFRFF 2 L 1 FFLFRFRFFLFLFRFF 3 R 1 FFRFLFLFFRFRFLFF 2 R 1 FFRFLFLFFRFRFLFF 2 L 1 FFFFFF 3 R 1 FFFRFLFLFRFF 5 R 1 FFLFRFRFLFFF 1 L 1 FFLFRFRFFLFLFRFF 2 R 1 FFRFLFLFFRFRFLFF 2 L 1",
    "10 FFLFRFRFFLFLFRFF 5 L 1 FFFRFLFLFRFF 4 L 1 FFLFRFRFFLFLFRFF 8 L 1 FFLFRFRFFLFLFRFF 4 L 1 FFFFFF 3 R 1"
  ]

main = mapM_ solution inputs
