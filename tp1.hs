menorDivisorDe n 1 = 1
menorDivisorDe n k | mod n k == 0 = k 
                   | otherwise = menorDivisorDe n (k+1)

menorDivisor n = menorDivisorDe n 2

-- esPrimo :: Int -> Bool
-- esPrimo n | menorDivisor n == n 
--           | otherwise = False

sonCoprimos :: Int -> Int -> Bool
sonCoprimos a p | a == 1 || p == 1 = True
                | menorDivisor p == p && mod (a*(p-1)-1) p == 0 = True
                | menorDivisor a == a && mod (p*(a-1)-1) a == 0 = True
                | otherwise = False 


