-- Trabalho 3
-- Alexandre Rodrigues 54472

module Root.Src.Trabalho3 where


-- Exercício A
-- Alínea 1
listaDeAlguidaresVazia :: [a]
listaDeAlguidaresVazia = []

-- Alínea 2
adicionaAListaDeAlguidares :: Ord a => Int -> a -> [[a]] -> [[a]]
adicionaAListaDeAlguidares _ elemento [] = [[elemento]]
adicionaAListaDeAlguidares tamanho elemento (x:xs)
    | length x < tamanho = (adicionaAAlguidar elemento x):xs -- se o tamanho da lista excede o tamanho máximo esta fica com um elemento a mais 
    | otherwise = init alguidar: adicionaAListaDeAlguidares tamanho (last alguidar) xs -- adiciona ao último alguidar o elemento a maisno alguidar anterior
    where alguidar = adicionaAAlguidar elemento x

adicionaAAlguidar :: Ord a => a -> [a] -> [a]
adicionaAAlguidar elemento [] = [elemento]
adicionaAAlguidar elemento (x:xs)
    | x < elemento = x:adicionaAAlguidar elemento xs -- adiciona o elemento depois do primeiro elemento do alguidar se for superior
    | otherwise = elemento:x:xs  -- adiciona o elemento antes do primeiro elemento do alguidar se for inferior

-- Alínea 3
elemListaDeAlguidares :: Ord a => a -> [[a]] -> Bool
elemListaDeAlguidares elemento lista =  foldr (\acc x -> acc || x) False (map (elem elemento) lista) -- determina se o elemento se encontra na lista de alguidares

-- Alínea 4
removerDaListaDeAlguidares :: Ord a => a -> [[a]] -> [[a]]
removerDaListaDeAlguidares elemento = map (filter (\x -> x /= elemento))
 
-- Alínea 5
fromList :: Ord a => Int -> [a] -> [[a]]
fromList tamanho = foldl (\acc x -> adicionaAListaDeAlguidares tamanho x acc) listaDeAlguidaresVazia -- converte uma dada lista numa lista de alguidares

-- Alínea 6
mapListaDeAlguidares :: (Ord a, Ord b) => Int -> (a -> b) -> [[a]] -> [[b]]
mapListaDeAlguidares tamanho funcao lista = fromList tamanho (map funcao (foldl (\acc e -> e ++ acc) [] lista))  -- retorna uma nova lista de alguidares com a aplicação da função a cada elemento da lista antiga


-- Exercício B
-- Alínea 1
createFastCache :: (Ord a, Ord b) => Int -> [a] -> [b] -> [[(a, b)]]
createFastCache tamanho lista1 lista2 = fromList tamanho (zip lista1 lista2) -- recebe o tamanho máximo das sub-listas, e duas listas de chaves e valores, em que cada posição na lista de chaves corresponde à mesma posição na lista de valores 

-- Alínea 2
fastGet :: (Ord a, Ord b) => [[(a, b)]] -> a -> [b]
fastGet lista chave = foldr (\e acc -> if ((fst e) == chave) then (snd e):acc else acc) [] (foldr (\e acc -> e ++ acc) [] lista) -- devolve uma lista com os vários valores associados da chave na lista de alguidares
