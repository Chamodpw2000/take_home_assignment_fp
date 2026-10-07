-- =====================================================================
-- EC8206 Functional Programming  |  University of Ruhuna
-- Workshop: Higher-Order Functions  (approx. 25 minutes in class)
--
-- Instructions:
--   1. Load this file:        ghci 02_LabExercises.hs
--   2. Replace each `undefined` with your own implementation.
--   3. Reload with `:r` and check with the sample tests: runTests
--
-- Rules: do NOT use explicit recursion in Part B — use map / filter /
-- folds / partial application instead. That is the point!
-- =====================================================================
  
module Main where
import Data.Char (toUpper)

-- ---------------------------------------------------------------------
-- PART A: Warm-up — functions as values           [Target: 5 minutes]
-- ---------------------------------------------------------------------

-- A1. Write applyThrice, which applies a function three times.
--     applyThrice (+2) 1   =>  7
applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (f (f x))

-- A2. Write pipeline, which applies a LIST of functions left to right.
--     pipeline [(+1), (*2)] 5   =>  12       (first +1, then *2)
--     Hint: recursion over the list of functions is allowed here,
--           or try foldl if you feel brave.
pipeline :: [a -> a] -> a -> a
pipeline fs x = foldl (\acc f -> f acc) x fs

-- ---------------------------------------------------------------------
-- PART B: map, filter, fold                       [Target: 12 minutes]
-- ---------------------------------------------------------------------

-- B1. Convert a list of temperatures in Celsius to Fahrenheit.
--     Formula: f = c * 9 / 5 + 32
--     toFahrenheit [0, 100]   =>  [32.0, 212.0]
toFahrenheit :: [Double] -> [Double]
toFahrenheit = map (\c -> c * 9 / 5 + 32)

-- B2. Keep only the passing marks (mark >= 40) from a list.
--     passing [35, 40, 88, 12]   =>  [40, 88]
passing :: [Int] -> [Int]
passing = filter (>= 40)

-- B3. Count how many elements satisfy a predicate — WITHOUT recursion.
--     countIf even [1..10]   =>  5
--     Hint: filter, then length. Or a fold.
countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

-- B4. Implement `myMaximum` for a non-empty list using foldr or foldl.
--     myMaximum [3, 9, 4]   =>  9
--     Hint: use `max` as the combining function; think about what the
--     starting value should be (try foldr1 / foldl1, or use head).
myMaximum :: [Int] -> Int
myMaximum = foldr1 max

-- B5. Using a single fold, compute the sum AND the count in one pass,
--     returning them as a pair. Then use it to write `average`.
--     sumAndCount [10, 20, 30]   =>  (60, 3)
sumAndCount :: [Int] -> (Int, Int)
sumAndCount = foldr (\x (s, c) -> (s + x, c + 1)) (0, 0)

average :: [Int] -> Double
average xs = fromIntegral s / fromIntegral c
  where (s, c) = sumAndCount xs

-- ---------------------------------------------------------------------
-- PART C: Currying & partial application          [Target: 8 minutes]
-- ---------------------------------------------------------------------

-- C1. Using PARTIAL APPLICATION ONLY (no lambda, no named parameter),
--     define a function that adds 18% VAT to a price.
--     addVAT 100   =>  118.0
addVAT :: Double -> Double
addVAT = (* 1.18)

-- C2. Define `startsWithA` using partial application of a library
--     function, to keep only words beginning with 'A'.
--     startsWithA ["Apple", "Ball", "Ant"]   =>  ["Apple", "Ant"]
--     Hint: filter, head, (==) ... or ((== 'A') . head)
startsWithA :: [String] -> [String]
startsWithA = filter ((== "A") . take 1)

-- C3. CHALLENGE: student records are (name, mark) pairs.
--     Produce the names of students who passed (mark >= 40),
--     in UPPERCASE, using one map+filter pipeline.
--     honourBoard [("amali", 78), ("kasun", 35)]   =>  ["AMALI"]
--     Hint: import Data.Char (toUpper) mentally — or use
--           map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)
honourBoard :: [(String, Int)] -> [String]
honourBoard = map (map toUpper . fst) . filter ((>= 40) . snd)

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
  check "A1 applyThrice" (applyThrice (+ 2) 1) 7
  check "A2 pipeline"    (pipeline [(+ 1), (* 2)] 5) 12
  putStrLn "Part B"
  check "B1 toFahrenheit" (toFahrenheit [0, 100]) [32.0, 212.0]
  check "B2 passing"      (passing [35, 40, 88, 12]) [40, 88]
  check "B3 countIf"      (countIf even [1 .. 10 :: Int]) 5
  check "B4 myMaximum"    (myMaximum [3, 9, 4]) 9
  check "B5 sumAndCount"  (sumAndCount [10, 20, 30]) (60, 3)
  check "B5 average"      (average [10, 20, 30]) 20.0
  putStrLn "Part C"
  check "C1 addVAT"       (addVAT 100) 118.0
  check "C2 startsWithA"  (startsWithA ["Apple", "Ball", "Ant"]) ["Apple", "Ant"]
  check "C3 honourBoard"  (honourBoard [("amali", 78), ("kasun", 35)]) ["AMALI"]

main :: IO ()
main = runTests
