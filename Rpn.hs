step :: [Int] -> String -> [Int]
step (x:y:ys) "+" = x + y:ys
step (x:y:ys) "-" = (y - x):ys
step (x:y:ys) "*" = (x * y):ys
step (x:y:ys) "/" = (y `div` x):ys
step (x:y:ys) "^" = (x ^ y):ys
step x numberString = [read numberString::Int] ++ x

rpn :: [String] -> Int
rpn = head . foldl step []

rpnFull :: [String] -> Int
rpnFull xs = head(foldl step [] xs)

rpnRec :: [String] -> Int
rpnRec [] = error "Input List is Empty"
rpnRec ys = head(rpnRecIntermediate [] ys)
  where rpnRecIntermediate x [y] = step x y
        rpnRecIntermediate x ys = rpnRecIntermediate (step x (head ys)) (tail ys)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
                    in foldl' f z' xs

stepPol :: [Int] -> String -> [Int]
-- This function is a sub function written for 2.2, Polish Notation. This relies
-- on the fact that if it is an operator, it must perform an operation on the
-- first two elements. otherwise it adds the number to the stack. the input is reversed
-- as the step function adds the stuff at the front of the stack like RPN, but
-- by reversing the input in the pn function this effect has been avoided.
stepPol ys x
  | x `elem` ["+","-","*"] = operation x ys
  | otherwise = read x : ys
  where
    operation op (y:x:ys) = case op of
      "+" -> x + y:ys
      "-" -> y - x:ys
      "*" -> x * y:ys

pn :: [String] -> Int
-- This repeatedly calls the stepPol function by reversing the input and then
-- extracting the first two integers in the list and then performing the
-- operations until it reaches the end and then extracts the integer which is
-- the only object in the list and returns it
pn s = head (foldl' stepPol [] (reverse s))

data RPNOut = Success Int | Stuck [Int] [String] | Incomplete [Int] deriving (Show)

-- step3 :: [Int] -> String -> Maybe [Int]
-- step3 xs x
--   |  x `elem` ["+","-","*"] = Just(operation x xs)
--   |  x `elem` ["0","1","2","3","4","5","6","7","8","9"] = Just(read x:xs)
--   | otherwise = Nothing
--   where
--     operation op (y:x:xs) = case op of
--       "+" -> x + y:xs
--       "-" -> y - x:xs
--       "*" -> x * y:xs

step3 :: [Int] -> String -> Maybe [Int]
step3 xs x
  | x `elem` ["+", "-", "*", "/", "^"] = (if length(xs) > 1 then (Just(operation x xs)) else (Nothing))
  | not(x `elem` ["+", "-", "*", "/", "^"]) = Just(read x:xs)
  where
    operation op (x:y:ys) = case op of
      "+" -> x + y:ys
      "-" -> y - x:ys
      "*" -> x * y:ys
      "/" -> (y `div` x):ys
      "^" -> (x ^ y):ys

rpn3 :: [String] -> RPNOut
rpn3 xs = (if length(arrRPN xs) == 1 then Success (fnRPN xs) else (if nOperands xs > nOperators xs then Incomplete (arrRPN xs) else Stuck (arrRPN (arrValid xs)) (arrRemain xs)))
  where nOperators xs = length ( [ x | x <- xs, x `elem` ["+", "-", "*", "/", "^"] ] )
        nOperands xs  = length ( [ x | x <- xs, not(x `elem` ["+", "-", "*", "/", "^"]) ] )
        nValid xs     = 2 * nOperands xs - 1
        arrValid xs   = take (nValid xs) xs
        arrRemain xs  = drop (nValid xs) xs
        arrRPN xs     = foldl step [] xs
        fnRPN xs      = head(arrRPN xs)

fact :: Int -> Int
fact n | n < 2     = 1
      | otherwise = n * fact (n-1)

-- sum' :: [Int] -> Int
-- sum' xs = head . sum xs
--   -- [x +y]: sum'(xs)


-- SAMPLE INPUTS:-

-- step[1, 3, 5, 7] "+"
--
-- rpn["1"]
-- rpn["1", "3", "+"]
-- rpn["1", "3", "+", "5", "+"]
-- rpn["1", "3", "5", "+", "+"]
-- rpn["1", "3", "5", "7", "+", "+", "+"]
-- rpn["21","5","1","1","+","+","*","3","*","2","2","+","-"]
--
-- rpnRec["1"]
-- rpnRec["1", "3", "+"]
-- rpnRec["1", "3", "+", "5", "+"]
-- rpnRec["1", "3", "+", "5", "+", "7", "+"]
-- rpnRec["1", "3", "+", "5", "+", "7", "+", "9", "+"]
-- rpnRec["1", "3", "5", "+", "+"]
-- rpnRec["1", "3", "5", "+", "+"]
-- rpnRec["1", "3", "5", "+"]
-- rpnRec["1", "3", "5", "7", "+", "+", "+"]
-- rpnRec["21","5","1","1","+","+","*","3","*","2","2","+","-"]
--
-- pn ["+","+","*","*","+","-","21","5","1","1","3","2","2"]
--
--
-- rpn3["21","5","1","1","+","+","*","3","*","2","2","+","-"]
-- rpn3["21","5","1","1","+","+","*","3","*","2","2","+"]
-- rpn3["21","5","1","1","+","+","*","3","*","2","2","+","-","-"]
--
-- step3[1, 3, 5, 7] "+"
