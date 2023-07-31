-- Trabalho 4
-- Alexandre Rodrigues 54472
module Eleicoes(Candidato(..), Estado(..), Nacao(..), criaNacao,
                        obterEstado, adicionaVotosEstado, adicionaVotosNacao,
                        vencedorEstado, vencedorEleicao) where

-- Alínea 1
data Candidato = A | B deriving (Eq, Show) -- representação do candidato

data Estado = Estado { nome :: String      -- representação do estado
                     , peso :: Int  
                     , votosA :: Int
                     , votosB :: Int
                     }                     

type Nacao = [Estado]                       -- representação da nação

-- Alínea 2
criaNacao :: [(String,Int)] -> Nacao
criaNacao [] = []
criaNacao ((nome, peso):xs) = Estado nome peso 0 0 : criaNacao xs -- cria uma lista de pares com o nome da nação e o peso dos votos, começando com 0 votos

-- Alínea 3
obterEstado :: Nacao -> String -> Estado
obterEstado [] estado = error("Não foi encontrado o estado " ++ estado)
obterEstado (e@(Estado nome _ _ _):xs) estado
    | nome == estado = e                        -- se o nome equivale ao nome do estado retorna esse estado
    | otherwise = obterEstado xs estado         -- caso contrário obtém-se um estado da lista de estados

-- Alínea 4
adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado estado a b = estado {votosA = (votosA estado) + a, votosB = (votosB estado) + b} -- adiciona o número de votos aos candidatos A e B aos respetivos candidatos

-- Alínea 5
adicionaVotosNacao :: Nacao -> [(String, Int, Int)] -> Nacao
adicionaVotosNacao nacao votos = map (funcAux votos) nacao

funcAux ::  [(String, Int, Int)] -> Estado -> Estado    -- função auxiliar da função adicionaVotosNacao
funcAux [] e = e
funcAux ((n, va, vb):xs) e =  if n == nome e then funcAux xs (adicionaVotosEstado e va vb) else funcAux xs e    -- adiciona os votos aos respetivos estados e devolve a nação com o número de votos atualizados
    
-- Alínea 6
vencedorEstado :: Estado -> Maybe Candidato
vencedorEstado  estado
    | votosA estado < votosB estado = Just B    -- o candidato B é vencedor num estado se tiver mais votos que o candidato A nesse estado
    | votosA estado > votosB estado = Just A    -- o candidato A é vencedor num estado se tiver mais votos que o candidato B nesse estado
    | otherwise = Nothing                       -- se nunhum dos dois acontecimentos anteriores se produzir, ocorre um empate entre os candidatos

-- Alínea 7
vencedorEleicao :: Nacao -> Maybe Candidato
vencedorEleicao nacao
    | vA < vB = Just B          -- se o candidato B alcançar mais representantes na nação que o candidato A, o candidato B vence a eleição
    | vA > vB = Just A          -- se o candidato A alcançar mais representantes na nação que o candidato B, o candidato A vence a eleição
    | otherwise = Nothing       -- se nenhum dos dois acontecimentos anteriores se produzir, ocorre um empate a nível nacional
    where (vA, vB) = contarVotos $ zip (map vencedorEstado nacao) (map (\x -> peso x) nacao)


contarVotos :: [(Maybe Candidato, Int)] -> (Int, Int)   -- função auxiliar da função vencedorEleicao
contarVotos [] = (0, 0)
contarVotos ((Just A, n):xs) = (l+n, r)         -- conta os votos casso ocorra JustA
    where (l, r) = contarVotos xs
contarVotos ((Just B, n):xs) = (l, r+n)         -- conta os votos caso ocorra JustB
    where (l, r) = contarVotos xs
contarVotos ((Nothing, _):xs) = contarVotos xs  -- conta os votos caso ocorra Nothing           

-- Alínea 8
instance Eq Estado where
    (==) e1 e2 = peso e1 == peso e2 && vencedorEstado e1 == vencedorEstado e2   -- se dois estados tiverem o mesmo peso e o mesmo vencedor, consideram-se iguais

-- Alínea 9
instance Show Estado where
    show e = nome e ++ " " ++ show (peso e) ++ " " ++ show (votosA e) ++ " " ++ show (votosB e)   -- representação textual do estado contendo o seu nome, peso e número de votos para cada candidato

