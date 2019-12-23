factorial :: Integer
          -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

read_maybe :: Read a => String 
                     -> Maybe a
read_maybe s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing

str_before_nth :: Char
               -> Double
               -> String
               -> Maybe String
str_before_nth c n "" = Nothing
str_before_nth c n (x:xs) | c == x && n == 1 = Just ""
                          | c == x           = str_before_nth c (n - 1) xs >>= \a -> Just $ [x] ++ a
                          | otherwise        = str_before_nth c n       xs >>= \a -> Just $ [x] ++ a

str_after_nth :: Char
              -> Double
              -> String
              -> Maybe String
str_after_nth c n "" = Nothing
str_after_nth c n (x:xs) | c == x && n == 1 = Just xs
                         | c == x           = str_after_nth c (n - 1) xs
                         | otherwise        = str_after_nth c n xs

str_split_nth :: Char
              -> Double
              -> String
              -> Maybe (String, String)
str_split_nth c n s = str_before_nth c n s >>= \a -> str_after_nth c n s
                                           >>= \b -> Just (a, b)

attempt_splits' :: (String -> Maybe Double) 
                -> (String -> Maybe Double)
                -> Char
                -> Double
                -> String
                -> Maybe (Double, Double)
attempt_splits' f1 f2 c n s | Just (s1, s2) <- str_split_nth c n s 
                            = case f1 s1 >>= \x -> f2 s2 
                                         >>= \y -> Just (x, y) of
                                   Just (a, b) -> Just (a, b)
                                   Nothing     -> attempt_splits' f1 f2 c (n + 1) s
                            | otherwise = Nothing

attempt_splits :: (String -> Maybe Double) 
               -> (String -> Maybe Double)
               -> Char
               -> String
               -> Maybe (Double, Double)
attempt_splits f1 f2 c s = attempt_splits' f1 f2 c 1 s

--  S :  E + E | E - E | E
use_S :: String
      -> Maybe Double
use_S "" = Nothing
use_S s | Just a      <- use_E s                              = Just a
        | Just (a, b) <- attempt_splits (use_S) (use_S) '+' s = Just (a + b)
        | Just (a, b) <- attempt_splits (use_S) (use_S) '-' s = Just (a - b)
        | otherwise                                           = Nothing

--  E :  F | E * E | E / E 
use_E :: String
      -> Maybe Double
use_E "" = Nothing
use_E s | Just a      <- use_F s                              = Just a
        | Just (a, b) <- attempt_splits (use_E) (use_E) '*' s = Just (a * b)
        | Just (a, b) <- attempt_splits (use_E) (use_E) '/' s = Just (a / b)
        | otherwise                                           = Nothing

--  F :  P | N | I!
use_F :: String
      -> Maybe Double
use_F "" = Nothing
use_F s | Just a <- use_P s = Just a
        | Just a <- use_N s = Just a
        | last s == '!'     = use_I (init s) >>= \x -> Just (fromIntegral (factorial x))
        | otherwise         = Nothing

--  P :  (S)
use_P :: String
      -> Maybe Double
use_P "" = Nothing
use_P [x] = Nothing
use_P (x:xs) | x == '(' && (last xs) == ')' = use_S (init xs)
             | otherwise                    = Nothing

--  N : real numbers
use_N :: String
      -> Maybe Double
use_N = read_maybe

--  N : integers
use_I :: String
      -> Maybe Integer
use_I = read_maybe

run_expression :: String
               -> Maybe Double
run_expression = use_S


main = do putStrLn "Enter your expression. (No white space)"
          ex <- getLine
          case run_expression ex of 
               Just rs -> do putStrLn $ "=" ++ show rs
               Nothing -> do putStrLn $ "!"