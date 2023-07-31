-- Trabalho 6
-- Alexandre Rodrigues 54472
import System.IO
import Control.Monad ( when )
import System.Exit ( exitSuccess )

type Coluna = Int
type Chave = [Int]

type N = Int

data Acumulador = GroupBy [(Chave, Int, Int)] | Val Int Int deriving Show
data Metrica = SUM Coluna [Coluna] Acumulador | MAX Coluna [Coluna] Acumulador | AVG Coluna [Coluna] Acumulador 
    deriving Show

getAcumulador :: Acumulador -> Chave -> (Int, Int)      -- obtém o acumulador
getAcumulador (GroupBy []) _ = (0, 0)
getAcumulador (GroupBy ((c, valor, count):xs)) chave = if chave == c then (valor, count) else getAcumulador (GroupBy xs) chave
getAcumulador (Val x y) _ = (x, y)

setAcumulador :: Acumulador -> Chave -> Int -> Int -> Acumulador    -- implementa um acumulador
setAcumulador (Val _ _) _ x y = Val x y
setAcumulador (GroupBy []) chave valor count = GroupBy [(chave, valor, count)]
setAcumulador (GroupBy lst) chave valor count = GroupBy ((chave, valor, count) : filter (\ (c, _, _) -> c /= chave) lst)

getMetrica :: Metrica -> Chave -> Float     -- obtém uma métrica
getMetrica (SUM _ _ acumulador) chave = fromIntegral (fst $ getAcumulador acumulador chave) :: Float
getMetrica (MAX _ _ acumulador) chave = fromIntegral (fst $ getAcumulador acumulador chave) :: Float
getMetrica (AVG _ _ acumulador) chave = (fromIntegral valor :: Float) / (fromIntegral count :: Float)
    where (valor, count) = getAcumulador acumulador chave

getChave :: [Int] -> [Int] -> Chave     -- obtém uma chave
getChave [] _ = []
getChave (x:xs) lst = lst !! x : getChave xs lst

getGroupBy :: Metrica -> [Int]  -- obtém um groupby
getGroupBy (SUM _ g _) = g
getGroupBy (MAX _ g _) = g
getGroupBy (AVG _ g _) = g

process :: Metrica -> [Int] -> Metrica  --implementação doo processo onde se emite a métrica calculada a partir das transações obtidas anteriormente
process (SUM col groupby acc) lst = SUM col groupby (setAcumulador acc chave (valor  + (lst !! col)) (count + 1))
    where chave = getChave groupby lst
          (valor, count) = getAcumulador acc chave
process (MAX col groupby acc) lst = if valor < lst !! col then MAX col groupby (setAcumulador acc chave (lst !! col) (count + 1)) else MAX col groupby (setAcumulador acc chave valor (count + 1))
    where chave = getChave groupby lst
          (valor, count) = getAcumulador acc chave
process (AVG col groupby acc) lst = AVG col groupby (setAcumulador acc chave (valor + (lst !! col)) (count + 1))
    where chave = getChave groupby lst
          (valor, count) = getAcumulador acc chave 


parseMetrica :: String -> Metrica   --emite a métrica
parseMetrica s
    | x == "sum" && null xs = SUM (read y :: Int) [] (Val 0 0)
    | x == "maximum" && null xs = MAX (read y :: Int) [] (Val 0 0)
    | x == "average" && null xs = AVG (read y :: Int) [] (Val 0 0)
    | x == "sum" && xs /= [] = SUM (read y :: Int) (parseGroupBy xs) (GroupBy [])
    | x == "maximum" && xs /= [] = MAX (read y :: Int) (parseGroupBy xs) (GroupBy [])
    | otherwise = AVG (read y :: Int) (parseGroupBy xs) (GroupBy [])
    where x:y:xs = words s

parseGroupBy :: [String] -> [Int]   -- emite um groupby
parseGroupBy lst = map (\ x -> read x :: Int) $ filter (/= "groupby") lst

parseValues :: String -> [Int]  -- emite os valores
parseValues lst = map (\ x -> read x :: Int) $ words lst

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    metrica <- getLine
    loop $ parseMetrica metrica

loop :: Metrica -> IO()     -- loop que repete o processo onde se emite a métrica calculada a partir das transações obtidas anteriormente
loop metrica = do
    line <- getLine
    when (line == "exit") exitSuccess
    let valores = parseValues line
    let chave = getChave (getGroupBy metrica) valores
    let nova = process metrica valores
    putStr (show (getMetrica nova chave) ++ "\n")
    loop nova

