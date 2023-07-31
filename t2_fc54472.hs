-- Trabalho 2
-- Alexandre Rodrigues 54472

module Root.Src.Trabalho2 where


-- Exercício A
-- Alínea 1
paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos [a,b] = [(a,b)] -- devolve as duas unidades consecutivas em forma de pares
paresConsecutivos (a:b:r) = (a,b):paresConsecutivos (b:r) -- devolve as várias unidades consecutivas em forma de pares
paresConsecutivos [_] = [] -- devolve uma lista vazia quando se introduz uma lista com uma unidade
paresConsecutivos [] = [] -- devolve uma lista vazia quando se introduz uma lista vazia

-- Alínea 2
diferencasConsecutivas :: [Int] -> [Int]
diferencasConsecutivas [] = [] -- devolve uma lista vazia quando se introduz uma lista vazia
diferencasConsecutivas [_] = [] -- devolve uma lista vazia quando se introduz uma unidade
diferencasConsecutivas (x1:x2:xs) = (x2-x1):diferencasConsecutivas(x2:xs) -- devolve uma lista com as diferenças entre cada elemento e o anterior quando se introduz uma lista de inteiros

-- Exercício B
-- Alínea 1
claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior (_, _, _) [] = False -- um jogador não é claramente pior que uma lista vazia
claramentePior jogador@(_, ataque, defesa) ((_, a, d):jogadores)
        | ataque < a && defesa < d = True -- se o ataque e defesa é claramente pior que no caso dos outos jogadores
        | otherwise = claramentePior jogador jogadores

-- Alínea 2
filtroJogadores :: [(String, Int, Int)] -> [(String, Int, Int)]
filtroJogadores jogadores = filtroJogadores' jogadores jogadores

filtroJogadores' :: [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)]  -- função auxiliar
filtroJogadores' _ [] = []
filtroJogadores' jogadores (jogador:restantes) -- cria uma lista dos restantes jogadores para além do primeiro jogador
    | claramentePior jogador jogadores = filtroJogadores' jogadores restantes -- devolve os jogadores que não são claramente piores em relação aos outros
    | otherwise = jogador : filtroJogadores' jogadores restantes

-- Exercício C

preencherVazio :: [[Int]] -> Int
preencherVazio (linha:linhas)
        | elem 0 linha = descobreValor linha -- descobre o valor em entre 1 e 9 na linha que substitui o 0
        | otherwise = preencherVazio linhas -- preenche os espaços vazios das linhas

descobreValor :: [Int] -> Int
descobreValor lista
    | not (elem 1 lista) = 1 -- insere elemento que não está contido na lista
    | not (elem 2 lista) = 2
    | not (elem 3 lista) = 3
    | not (elem 4 lista) = 3
    | not (elem 5 lista) = 5
    | not (elem 6 lista) = 6
    | not (elem 7 lista) = 7
    | not (elem 8 lista) = 8
    | otherwise = 9



