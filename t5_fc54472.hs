-- Trabalho 5
-- Alexandre Rodrigues 54472
import System.Environment
import Data.List
import Control.Monad
import System.Exit

-- Alínea A
main :: IO ()
main = do
    let filters = []
    args <- getArgs
    contents <- readFile $ head args    --  lê os dados de um ficheiro de texto
    loop filters contents

loop :: [String] -> String -> IO()  --  funciona como loop de repetição do processo
loop filters contents = do
    putStr $ unlines $ filter (\frase -> foldl (\acc f -> acc && isInfixOf f frase) True filters) $ lines contents  -- mostra as linhas do ficheiro que contêm a string inserida
    putStr $ "\nFiltering:" ++ foldl (\acc f -> f ++ " " ++ acc) "" filters ++ "\n> "   -- guarda as strings filtradas
    let oldFilters = filters
    filtro <- getLine
    when (filtro == "pop" && null filters) exitSuccess  --o último "pop" termina o programa
    let filters = if filtro == "pop" then take (length oldFilters - 1) oldFilters else filtro:oldFilters    --desempilha o último filtro inserido
    loop filters contents