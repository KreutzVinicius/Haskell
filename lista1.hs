concatena :: [a] -> [a] -> [a]

concatena [] ys =
 ys
concatena (x:xs) ys = 
 x : concatena xs ys

--
pertence :: Eq a => a -> [a] -> Bool

pertence _ [] =
 False
pertence t (x:xs) =
 if x == t then True 
 else pertence t xs

--
intersecao :: Eq a => [a] -> [a] -> [a]

intersecao [] ys =
 []
intersecao xs [] = 
 []
intersecao (x:xs) ys= 
 if pertence x ys then x : intersecao xs ys
 else intersecao xs ys
 
--
inverso :: [a] -> [a]

inverso [] = 
  []
inverso (x:xs) =
  concatena (inverso xs) [x]
  
-- 
primeiros :: Int -> [a] -> [a]

primeiros _ [] = 
 []
primeiros 0 x =
 []
primeiros n (x:xs) =
 x : primeiros (n-1) xs
 
--
ultimos :: Int -> [a] -> [a]
 
ultimos _ [] = 
  []
ultimos 0 x =
  []
ultimos n x =
  inverso (primeiros n (inverso x))  

--
tamanho :: [a] -> Int

tamanho [] =
  0
tamanho (x:xs) = 
  1 + tamanho xs


--
binParaInt :: String -> Int 

binParaInt [] = 
 0
binParaInt ('0':xs) = 
 binParaInt xs
binParaInt ('1':xs) =
 2^tamanho xs + binParaInt xs

--
intParaBin :: Int -> String

intParaBin 0 = []
intParaBin x =
 if (x `mod` 2) == 0 then
  '0': intParaBin (x`div`2)
  else
   '1': intParaBin (x`div`2)


--
menorValor :: Ord a => [a] -> a
menorValor [x] =
 x
menorValor (x:y:xs) =
 if x < y then 
  menorValor (x:xs)
  else
   menorValor (y:xs)

-- remove a primeira ocorrencia de um elemento da lista
removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro [] _ = 
 []
removerPrimeiro (x:xs) a =
 if x == a then
  xs
 else
  x : removerPrimeiro xs a


--ordem crescente
ordenar :: Ord a => [a] -> [a]
ordenar [x] =
 [x]
ordenar (x:xs) = 
 if x == menorValor (x:xs) then
  x: ordenar xs
  else
   ordenar (concatena xs [x]) 

--
impares :: [Int] -> [Int]
impares [x] =
 if x `mod` 2 == 1 then
 [x]
 else
  []
impares (x:xs) = 
 if x `mod` 2 == 1 then
 x : impares xs
 else
  impares xs
