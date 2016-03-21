--Exercicio1
somaquad :: Int -> Int -> Int
somaquad a b = a*a + b*b

--Exercicio2
hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads a b = if (head a) == (head b) then True else False

--Exercicio3
Srinicio :: [String] -> [String]
Srinicio a = map ("Sr."++) a

--Exercicio4
strNum :: String -> Int
strNum a = length (filter (==' ') a)

--Exercicio5
calculaNum :: [Double] -> [Double]
calculaNum n = map (\n -> 3*n^2 + 2/n + 1) n

--Exercicio6
NegativNum :: [Int] -> [Int]
NegativNum n = filter (\n -> n < 0) n

--Exercicio7
EntreCem :: [Int] -> [Int]
EntreCem n = filter (\n -> n > 0 && n < 101) n

--Exercicio8
Idade70 :: [Int] -> [Int]
Idade70 n = filter (\n -> n < 46) n

--Exercicio9
RetPares :: [Int] -> [Int]
RetPares n = filter (\n -> mod n 2 == 0) n

--Exercicio10
charFound :: Char -> String -> Bool
charFound a s = any (== a) s

--Exercicio11
--takeWhile (>5) [5,6,7,4,3]
--takeWhile (/= ' ') "Ciclano de Tal"
--takeWhile even [2,4,5,6,7]

--Exercicio12
TermA :: [String] -> [String]
TermA [] = []
TermA x = if (last (takeWhile(/= ' ')(head x)))=='a' then (head x) : findA(tail x) else findA(tail x)
