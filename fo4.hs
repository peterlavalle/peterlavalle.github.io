-- licesned as http://www.dbad-license.org/
import Data.List
import Test.QuickCheck

xp_next :: Int -> Int
xp_next level
    | level == 1 = 200
    | otherwise  = 75 * (level - 1) + 200

main = do
  quickCheck (275 == xp_next 2)
  quickCheck (350 == xp_next 3)
  quickCheck (1700 == xp_next 21)

-- crafting recipe definition
data Recipe = Recipe {xp::Int, junk::[(Junk, Int)]} deriving Show

-- types of junk which can be used to craft things
data Junk
  = Plastic
  | Fertilizer
  | Copper
  | FlamerFuel
  | Jet
  | FiberGlass
  | Oil
  | Steel
  | Cloth
  | Adhesive
  | Glass
  | Circuitry
  | Gear
  | Acid
  | Bone
  | PureWater
  | Corn
  | Mutifruit
  | Tato
  deriving (Show, Eq)

jet = Recipe
  13
  [ (Plastic, 1),
    (Fertilizer, 2),
    (Jet, -1)]

statue = Recipe
  36
  [ (Copper, 8)]


jetfuel = Recipe
  15
  [ (FlamerFuel, 5),
    (Jet, 1)]

bleedout = Recipe
  13
  [ (FiberGlass, 1),
    (Oil, 1),
    (Steel, 1)]

heavygun = Recipe
  36
  [ (Steel, 10 - 5),
    (Circuitry, 2),
    (Gear, 2),
    (Oil, 4)]

sleepbag = Recipe
  3
  [ (Cloth, 2)]

-- because fibrglass is oddly rare ...
moltov = Recipe
  16
  [ (Adhesive, 2),
    (Cloth, 1),
    (Glass, 2),
    (Oil, 4)]



cutting_fluid = Recipe
  14
  [ (Acid, 2),
    (Bone, 8),
    (PureWater, 2),
    (Steel, 4 - 1),
    (Oil, -3)]

vegetable_starch = Recipe
  16
  [ (Corn, 3),
    (Mutifruit, 3),
    (Tato, 3),
    (PureWater, 1),
    (Adhesive, 5)]


-- how much do I have?
start :: [(Int, Junk)]
start = [
  (2920, Steel),
  (254, Fertilizer),
  (1670, Plastic),
  (1326, FlamerFuel),
  (113, FiberGlass),
  (0, Acid),
  (0, Bone),
  (0, PureWater),
  (0, Corn),
  (0, Mutifruit),
  (0, Tato),
  (196, Oil),
  (90, Jet),  (496, Copper),
  (190, Adhesive),
  (1822, Glass),
  (2816, Cloth),
  (210, Circuitry),
  (655, Gear)]


-- can i cook this
recipe_possible :: [(Int, Junk)] -> Recipe -> Bool
recipe_possible inventory (Recipe _ junks) =

    foldl (&&) True (map has_at_enough junks)

  where

    has_at_enough :: (Junk, Int) -> Bool
    has_at_enough (j, c) =
      c < count_of j inventory

    count_of :: Junk -> [(Int, Junk)] -> Int
    count_of junk list = if junk == snd ( head list )
      then fst $ head list
      else count_of junk $ tail list


-- remove the recipe from our inventory
remove_recipe :: [(Int, Junk)] -> Recipe -> [(Int, Junk)]
remove_recipe inventory (Recipe _ junks) =
    map (\(c,j) -> (c - count_of j junks,j)) inventory
  where
    count_of :: Junk -> [(Junk, Int)] -> Int
    count_of _ [] = 0
    count_of junk list = if junk == fst ( head list )
      then snd $ head list
      else count_of junk $ tail list


possible_xp :: [(Int, Junk)] -> [Recipe] -> Int
possible_xp _ [] = 0
possible_xp inventory (recipe:recipes) =
  if recipe_possible inventory recipe
    then (xp recipe) + possible_xp (remove_recipe inventory recipe) (recipe:recipes)
    else possible_xp inventory recipes


recipes = [
  heavygun,
  jet,
  statue,
  jetfuel,
  bleedout,
  moltov,
  cutting_fluid,
  vegetable_starch,
  sleepbag]

possible_with_inventory :: Int
possible_with_inventory = possible_xp start recipes
needed_for_goal = sum $ map xp_next [64 .. (63+3)]

variations :: [Int]
variations = [
  possible_xp start order
  | order <- (permutations recipes) ]

variation_max = maximum variations

-- permutations :: [a] -> [[a]]
-- permutations [] = [ [] ]

-- no good ... but why?
-- permutations (head : (next : tail)) =
--  let
  --   first = -- every permutation of next : tail with head prefixed
  --   second = -- every permutation of next : tail with head prefixed?
  -- in
  --   first ++ second
--



hedmutations list =
  all_heads_lists
  where
    all_indicies = [0 .. (length list) - 1]

    all_heads_lists = [
      (list !! i, (take i list) ++ (drop (i+1) list))
      | i <- [0 .. (length list) - 1] ]




