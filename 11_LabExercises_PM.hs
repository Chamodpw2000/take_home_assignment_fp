-- =====================================================================
-- EC8206 Functional Programming  |  University of Ruhuna
-- Workshop: Pattern Matching  (approx. 25 minutes)
--
-- Instructions:
--   1. Load this file:        ghci 11_LabExercises_PM.hs
--   2. Replace each `undefined` with pattern-matching definitions.
--   3. Reload with `:r` and check with:  runTests
--
-- Rules: use PATTERNS (and guards) - not if/then/else chains, and not
-- == on whole structures. That is the point!
-- =====================================================================


module Main where

-- ---------------------------------------------------------------------
-- PART A: Values & wildcards                      [Target: 7 minutes]
-- ---------------------------------------------------------------------

-- A1. Write sinhalaDigit for 0, 1 and 2; everything else is "?".
--     sinhalaDigit 0 = "binduwa", 1 = "eka", 2 = "deka", _ = "?"
sinhalaDigit :: Int -> String
sinhalaDigit 0 = "binduwa"
sinhalaDigit 1 = "eka"
sinhalaDigit 2 = "deka"
sinhalaDigit _ = "?"

-- A2. Write isWeekendDay using String literal patterns for
--     "Sat" and "Sun"; a wildcard for the rest.
--     isWeekendDay "Sat"   =>  True
--     isWeekendDay "Mon"   =>  False
isWeekendDay :: String -> Bool
isWeekendDay "Sat" = True
isWeekendDay "Sun" = True
isWeekendDay _     = False

-- A3. Write startsWithA, matching the FIRST Char of a String with a
--     literal inside the (:) pattern. Empty strings give False.
--     startsWithA "Amali"  =>  True
--     startsWithA "Kasun"  =>  False
--     startsWithA ""       =>  False
startsWithA :: String -> Bool
startsWithA ('A' : _) = True
startsWithA ('a' : _) = True
startsWithA _         = False

-- ---------------------------------------------------------------------
-- PART B: Guards & structures                     [Target: 10 minutes]
-- ---------------------------------------------------------------------

-- B1. Write gradeOf using GUARDS (>= comparisons, otherwise):
--     75+ -> 'A',  65+ -> 'B',  55+ -> 'C',  40+ -> 'S',  else 'F'
gradeOf :: Int -> Char
gradeOf mark
  | mark >= 75 = 'A'
  | mark >= 65 = 'B'
  | mark >= 55 = 'C'
  | mark >= 40 = 'S'
  | otherwise  = 'F'

-- B2. Write quadrant for a coordinate pair, combining a tuple
--     PATTERN with GUARDS:
--     quadrant (3, 4)    =>  "I"      (x > 0, y > 0)
--     quadrant (-3, 4)   =>  "II"     (x < 0, y > 0)
--     quadrant (-3, -4)  =>  "III"    (x < 0, y < 0)
--     quadrant (3, -4)   =>  "IV"     (x > 0, y < 0)
--     anything on an axis => "axis"
quadrant :: (Int, Int) -> String
quadrant (x, y)
  | x > 0 && y > 0 = "I"
  | x < 0 && y > 0 = "II"
  | x < 0 && y < 0 = "III"
  | x > 0 && y < 0 = "IV"
  | otherwise      = "axis"

-- B3. Write secondElement using multi-element list patterns
--     (no length, no !!):
--     secondElement [7,8,9]  =>  Just 8
--     secondElement [7]      =>  Nothing
--     secondElement []       =>  Nothing
secondElement :: [a] -> Maybe a
secondElement (_ : y : _) = Just y
secondElement _           = Nothing

-- B4. Write sameFirstTwo: True when a list's first two elements are
--     equal. Lists with fewer than two elements give False.
--     Hint: pattern (x : y : _) plus a guard or ==.
--     sameFirstTwo [5,5,1]  =>  True
--     sameFirstTwo [5,4,5]  =>  False
sameFirstTwo :: Eq a => [a] -> Bool
sameFirstTwo (x : y : _) = x == y
sameFirstTwo _           = False

-- ---------------------------------------------------------------------
-- PART C: Recursive patterns on lists & trees     [Target: 8 minutes]
-- ---------------------------------------------------------------------

-- C1. Write countPositives recursively with patterns [] and (x:xs)
--     plus guards (no filter/length - we are practising recursion!).
--     countPositives [3, -1, 7, 0]  =>  2
countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x : xs)
  | x > 0     = 1 + countPositives xs
  | otherwise = countPositives xs

-- The tree type from the lecture:
data Tree = Leaf
          | Node Tree Int Tree
  deriving (Show, Eq)

sample :: Tree
sample =
  Node (Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf))
       5
       (Node Leaf 8 (Node Leaf 9 Leaf))

-- C2. Write countLeaves: a Leaf counts 1; a Node counts the leaves
--     of both subtrees.
--     countLeaves sample  =>  7
countLeaves :: Tree -> Int
countLeaves Leaf         = 1
countLeaves (Node l _ r) = countLeaves l + countLeaves r

-- C3. Write treeMax for the LARGEST value in a tree; a Leaf
--     contributes minBound (the smallest possible Int).
--     treeMax sample  =>  9
treeMax :: Tree -> Int
treeMax Leaf         = minBound
treeMax (Node l v r) = treeMax l `max` v `max` treeMax r

-- C4. CHALLENGE: write mirror, which swaps left and right subtrees
--     all the way down.
--     toList (mirror sample)  =>  [9,8,5,4,3,1]
mirror :: Tree -> Tree
mirror Leaf         = Leaf
mirror (Node l v r) = Node (mirror r) v (mirror l)

-- (helper used by the C4 test)
toList :: Tree -> [Int]
toList Leaf         = []
toList (Node l v r) = toList l ++ [v] ++ toList r

-- ---------------------------------------------------------------------
-- Simple test harness — run `runTests` in GHCi
-- ---------------------------------------------------------------------

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name got want =
  putStrLn $
    (if got == want then "  PASS  " else "  FAIL  ")
      ++ name
      ++ "  (got " ++ show got ++ ", expected " ++ show want ++ ")"

runTests :: IO ()
runTests = do
  putStrLn "Part A"
  check "A1 sinhalaDigit 1" (sinhalaDigit 1) "eka"
  check "A1 sinhalaDigit 9" (sinhalaDigit 9) "?"
  check "A2 weekend"        (isWeekendDay "Sat") True
  check "A2 weekday"        (isWeekendDay "Mon") False
  check "A3 startsWithA"    (startsWithA "Amali") True
  check "A3 empty"          (startsWithA "") False
  putStrLn "Part B"
  check "B1 gradeOf"     (map gradeOf [82, 68, 58, 45, 12]) "ABCSF"
  check "B2 quadrant I"  (quadrant (3, 4)) "I"
  check "B2 quadrant III" (quadrant (-3, -4)) "III"
  check "B2 axis"        (quadrant (0, 5)) "axis"
  check "B3 second"      (secondElement [7, 8, 9 :: Int]) (Just 8)
  check "B3 too short"   (secondElement [7 :: Int]) Nothing
  check "B4 same"        (sameFirstTwo [5, 5, 1 :: Int]) True
  check "B4 different"   (sameFirstTwo [5, 4, 5 :: Int]) False
  putStrLn "Part C"
  check "C1 countPositives" (countPositives [3, -1, 7, 0]) 2
  check "C2 countLeaves"    (countLeaves sample) 7
  check "C3 treeMax"        (treeMax sample) 9
  check "C4 mirror"         (toList (mirror sample)) [9, 8, 5, 4, 3, 1]

main :: IO ()
main = runTests
