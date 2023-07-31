-- Trabalho 1
-- Alexandre Rodrigues 54472

module Root.Src.Trabalho1 where

frequencias :: String -> [Int]
frequencias s = [ sum [ 1 | c' <- s, c' == c] | c <- s]

pequenasPalavras :: [[Char]]
pequenasPalavras = [ [x, y, z] | x <- ['a'..'z'], y <- ['a'..'z'], z <- ['a'..'z'], vogalExiste x y z ]

-- Verifica se uma das letras é uma vogal
vogalExiste :: Char -> Char -> Char -> Bool
vogalExiste x y z = x `elem` ['a', 'e', 'i', 'o', 'u', 'y'] ||
                    y `elem` ['a', 'e', 'i', 'o', 'u', 'y'] ||
                    z `elem` ['a', 'e', 'i', 'o', 'u', 'y']

-- 10 4 [("Dto", 10), ("Esq", 8), ("Cent", 3)]
legendaCampainha :: Int -> Int -> [(String, Int)] -> [String]
legendaCampainha n1 n2 regras = if n1 >= n2 && n1 /= 0
                                then [ show andar ++ x | andar <- [1.. n1 + 1] ,(x, y) <- regras, if y >= n2 then andar <= y + 1 else andar <= y, andar /= n2]
                                else [ show andar ++ x | andar <- [1.. n1] ,    (x, y) <- regras, andar <= y]