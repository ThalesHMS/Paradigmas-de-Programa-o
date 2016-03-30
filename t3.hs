ex1)
elv2 :: [Int] -> [Int]
elv2 [] = []
elv2 a = (head a)*(head a) : elv2 (tail a)

ex2)
add :: [String] -> [String]
add [] = []
add (x:xs) = ("Sr. " ++ x) : add xs

ex3)
contaespaco :: String -> Int
contaespaco [] = 0
contaespaco (x:xs) = if (x == ' ') then (1 + contaespaco xs) else (0 + contaespaco xs)

ex4)
equacao :: [Float] -> [Float]
equacao [] = []
equacao (n:ns) = (3*n^2 + 2/n + 1) : equacao ns

ex5)
Negat :: [Int] -> [Int]
Negat [] = []
Negat (x:xs)
	| x < 0	= x : Negat xs
	| otherwise = Negat xs

ex6)
semVogais :: String -> String
semVogais [] = []
semVogais z = filter (\ z -> (z /= 'a') && (z /= 'e') && (z /= 'i') && (z /= 'o') && (z /= 'u')) x

ex7)
semVogais2 :: String -> String 
semVogais2 [] = []
semVogais2 (z:zs)
	| (z == 'a') || (z == 'e') || (z == 'i') || (z == 'o') || (z == 'u') = semVogais2 zs
	| otherwise = z : semVogais2 zs

ex8)
codifica :: String -> String
codifica [] = []
codifica a = map (code) a
	where code a
		| a == ' ' = ' '
		| otherwise = '-'

ex9)
codifica2 :: String -> String
codifica2 [] = []
codifica2 (a:as)
	| a == ' ' = ' ' : codifica2 as
	| otherwise = '-' : codifica2 as

ex10)
charFound :: Char -> String -> Bool
charFound _ [] = False
charFound a (b:bs)
	| a == b = True
	| otherwise = charFound a bs

ex11)
translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate (c:cs) = (fst(c)+2.0,snd(c)+2.0) : translate (cs)

ex12)
prodVet :: [Int] -> [Int] -> [Int]
prodVet _ [] = []
prodVet [] _ = []
prodVet (a:as) (b:bs) = (a*b) : prodVet as bs

ex13)
prodVet2 :: [Int] -> [Int] -> [Int]
prodVet2 a b = zipWith (*) a b

ex14)
geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = aux 1 n

aux :: Int -> Int -> [(Int,Int)]
aux a b
	| a <= b = (a,a^2) : aux (a+1) b
	| otherwise = []