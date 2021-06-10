--1
ends :: [Int] -> [Int]
ends [] = []
ends (x:xs) =  x : last xs : []

--2
deduzame' :: [Integer] -> [Integer]
deduzame' [] = []
deduzame' (x:xs) = (2 * x) : deduzame' xs

--3
deduzame2' :: [Integer] -> [Integer]
deduzame2' [] = []
deduzame2' (x:xs) = if x > 2
  then head xs : deduzame2' (tail xs) 
  else deduzame2' (tail xs)

--4
geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n,n^2) : geraTabela(n-1)

--5
contido :: Char -> String -> Bool
contido ch str
    |ch == (head str) = True
    |(tail str) == [] = False
    |otherwise = contido ch (tail str)

--6
translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = [] 
translate list = (fst (head list)+2,snd (head list)+2) : translate (tail list)

--7
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) 
  |length x >= 5 = 1 + countLongs xs
  |otherwise = countLongs xs

--8
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs str  
  |(length (head str) >= 5) = (head str) : onlyLongs (tail str)
  |otherwise = onlyLongs (tail str)
