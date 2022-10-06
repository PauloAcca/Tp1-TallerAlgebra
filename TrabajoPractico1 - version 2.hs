-- Trabajo Práctico 1


mayorDivisorDe :: Integer -> Integer -> Integer -- muestra los divisores de manera descendente
mayorDivisorDe n 1 = 1
mayorDivisorDe n k | mod n k == 0 = k
                   | otherwise = mayorDivisorDe n (k-1)

mayorDivisor n = mayorDivisorDe n (n-1) -- Mayor divisor omitiendo el n

esPrimo :: Integer -> Bool -- Muestra si un numero es primo
esPrimo n | mayorDivisor n == 1 = True
          | otherwise = False

comparacionDivisores :: Integer -> Integer -> Integer -> Integer -> Bool -- Compara si el único divisor en común entre n1 y n2 es el 1
comparacionDivisores n1 n2 k1 k2 | k1 == 1 && k2 == 1 = True
                                 | k1 == k2 = False
                                 | k1 == 1 = comparacionDivisores n1 n2 n1 (mayorDivisorDe n2 (k2-1))
                                 | otherwise = comparacionDivisores n1 n2 (mayorDivisorDe n1 (k1-1)) k2


sonCoprimos :: Integer -> Integer -> Bool -- EJERCICIO 1
sonCoprimos n p = comparacionDivisores n p n p


pequeñoTeoremaFermat a p = a^(p-1) - 1 -- Formula base del pequeño teorema de Fermat

esPseudoprimo :: Integer -> Integer -> Bool -- Muestra si un numero es pseudonumero
esPseudoprimo a p = (sonCoprimos a p && mod (pequeñoTeoremaFermat a p) p == 0) && esPrimo p == False


es2Pseudoprimo :: Integer -> Bool -- EJERCICIO 2
es2Pseudoprimo n = esPseudoprimo 2 n


cantidad3Pseudoprimo :: Integer -> Integer -- EJERCICIO 3
cantidad3Pseudoprimo 1 = 0
cantidad3Pseudoprimo m | esPseudoprimo 3 m == True = cantidad3Pseudoprimo (m-1) + 1
                       | otherwise = cantidad3Pseudoprimo (m-1) 


es3Pseudoprimo :: Integer -> Bool -- pseudonumeros de base 3
es3Pseudoprimo n = esPseudoprimo 3 n

pseudoprimosIguales :: Integer -> Bool -- Compara si 2 pseudonumeros son iguales
pseudoprimosIguales n = es2Pseudoprimo n && es3Pseudoprimo n

mostrarPseudoprimo :: Integer -> Integer -> Integer
mostrarPseudoprimo k m | k == 0 = m
                       | pseudoprimosIguales m == True = mostrarPseudoprimo (k-1) (m+1)
                       | otherwise = mostrarPseudoprimo k (m+1)


kesimo2y3Pseudoprimo :: Integer -> Integer -- EJERCICIO 4
kesimo2y3Pseudoprimo m = mostrarPseudoprimo m 1

pseudoPrimidadDescendiente :: Int -> Int -> Bool -- Revisa si n es pseudoprimo para todo número entre 1 y k
pseudoPrimidadDescendiente n 1 = True
pseudoPrimidadDescendiente n k | esPseudoprimo k n = pseudoPrimidadDescendiente n (k-1)
                               | not sonCoprimos n k = pseudoPrimidadDescendiente n (k-1)
                               | sonCoprimos k n && not esPseudoprimo k n = False

esCarmichael :: Int -> Bool -- EJERCICIO 5
esCarmichael n = pseudoPrimidadDescendiente n (n-1)
