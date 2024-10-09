module Challenge where

import Data.Function (on)
import Data.IntMap (disjoint)
import Data.List (groupBy, sort, sortBy, tails, transpose)
import Data.Ord (comparing)
import Data.Sequence (chunksOf)
import Prelude hiding (compare)

x = (!! 0)

y = (!! 1)

pairs [] = []
pairs (a : b : bs) = [a, b] : pairs bs

stretch [] = []
stretch (cmd : n : rest) = replicate (read n :: Int) cmd ++ stretch rest

actions = concat . stretch . tail . words

perform _ p [] = (p, [])
perform prevDir positions@(prevPos : _) (action : actions) =
  perform dir (pos : positions) actions
  where
    step 'F' pos dir = (zipWith (+) pos dir, dir)
    step 'L' pos [x, y] = (pos, [y, -x])
    step 'R' pos [x, y] = (pos, [-y, x])

    (pos, dir) = step action prevPos prevDir

positions = fst . perform [0, 1] [[0, 0]]

distance = length . filter (== 'F')

square =
  product
    . map ((-) . minimum <*> maximum)
    . transpose

solution =
  unwords
    . map show
    . mconcat (map (return .) [distance, square . positions, length . surface . positions])
    . actions

velocities = (<*> rotate 1) $ zipWith $ zipWith (-)
  where
    rotate = drop <> take

verticals =
  sortBy (comparing y <> comparing x)
    . map offset
    . filter ((/= 0) . y . snd)
    . (zip <*> velocities)
  where
    offset ([a, b], [_, 1]) = [a, b - 1]
    offset (pos, [_, _]) = pos

horizontals = map reverse . verticals . map reverse

line [[xa, y], [xb, _]] = map (vec2 y) [xa .. xb - 1]
  where
    vec2 y x = [x, y]

surface = concatMap line . pairs . verticals

-- enclosed = concatMap (concatMap create . (zip <*> drop 2) . pairs) . groupBy compare . verticals
--   where
--     create ([_, [bx, y]], [[cx, _], _]) = line [[bx, y], [cx, y]]

compare a b c = c a == c b

-- enclosed = concatMap (concatMap create . (zip <*> drop 2) . map pairs) . groupBy compareY . verticals
--   where

-- phi :: (b -> b -> c) -> (a -> b) -> (a -> b) -> a -> c (signature for the phi combinator)

create ([_, [bx, y]], [[cx, _], _]) = line [[bx, y], [cx, y]]

-- >>> map create . concatMap ((zip <*> drop 1) . pairs) . groupBy compare x . horizontals . positions . actions $ (inputs !! 1)
-- [([[-4,0],[-4,2]],[[-4,4],[-4,6]]),([[-3,0],[-3,2]],[[-3,4],[-3,6]])]

-- pockets x = []

area = sum . map dist . pairs . map x . verticals
  where
    dist [a, b] = b - a

inputs =
  [ "1 FFFR 4",
    "9 F 6 R 1 F 4 RFF 2 LFF 1 LFFFR 1 F 2 R 1 F 5",
    "14 L 1 FR 1 FFFFFL 1 FFFFL 1 F 12 L 1 F 12 L 1 F 12 L 1 FFFFL 1 FFFFFFFFR 1 FFFR 1 FFFL 1",
    "32 FFRFLFLFFRFRFLFF 3 R 1 FFLFRFRFLFFF 3 R 1 FFFFFF 3 L 1 FFFRFLFLFRFF 2 R 1 FFFRFLFLFRFF 3 R 1 FFFFFF 1 L 1 FFRFLFLFFRFRFLFF 3 R 1 FFLFRFRFFLFLFRFF 2 L 1 FFLFRFRFFLFLFRFF 3 R 1 FFRFLFLFFRFRFLFF 2 R 1 FFRFLFLFFRFRFLFF 2 L 1 FFFFFF 3 R 1 FFFRFLFLFRFF 5 R 1 FFLFRFRFLFFF 1 L 1 FFLFRFRFFLFLFRFF 2 R 1 FFRFLFLFFRFRFLFF 2 L 1",
    "10 FFLFRFRFFLFLFRFF 5 L 1 FFFRFLFLFRFF 4 L 1 FFLFRFRFFLFLFRFF 8 L 1 FFLFRFRFFLFLFRFF 4 L 1 FFFFFF 3 R 1"
  ]

main = mapM_ (putStrLn . solution) inputs
