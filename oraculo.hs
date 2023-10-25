module Oraculo
    (crearOraculo,
    respuesta,
    readOraculo,
    Oraculo(..))
where

import qualified Data.Map as Map
    
{-- TIPOS DE DATOS --}

type Opciones = Map.Map String Oraculo

data Oraculo = OraculoPred { prediccion :: String}
             | OraculoPreg { pregunta :: String, opciones :: Opciones}
             deriving (Show, Read)

--Definicion de Oraculo como tipo equiparable
instance Eq Oraculo where
    (==) OraculoPreg {pregunta = _, opciones = _} OraculoPred {prediccion = _} = False
    (==) OraculoPred {prediccion = _} OraculoPreg {pregunta = _, opciones = _} = False
    (==) OraculoPreg {pregunta = p, opciones = _} OraculoPreg {pregunta = q, opciones = _} = p == q
    (==) OraculoPred {prediccion = p} OraculoPred {prediccion = q} = p == q


{-- FUNCIONES DE ACCESO --}

-- Función que devuelve el oráculo correspondiente a la respuesta
-- asociada al string S (el input) si este es una pregunta.
respuesta :: Oraculo -> String -> Oraculo
respuesta (OraculoPred _) _ = error "No se puede obtener respuesta de una prediccion"
respuesta (OraculoPreg _ o) s = o Map.! s

{--
obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
obtenerCadena (OraculoPred p) s = 
obtenerCadena (OraculoPreg p o) s =  
--}

{-- FUNCIONES DE CONSTRUCCIÓN --}

-- Recibe un string y devuelve un oráculo únicamente con ese string
crearOraculo :: String -> Oraculo
crearOraculo s = OraculoPred s

-- recibe una lista de strings, una de oráculos y un string,
-- devuelve un oráculo del tipo pregunta con el string y las opciones
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar sl ol s = OraculoPreg {pregunta = s, opciones = Map.fromList $ zip sl ol}

{-- INSTANCIAS --}
readOraculo :: String -> Oraculo
readOraculo s = read s :: Oraculo 

{-
Recibe un oraculo y una cadena de caracteres que representa una predicción. Retorna un valor
de tipo Maybe [(String, String)]. Si la predicción suministrada no pertenece al oráculo 
retorna Nothing, en caso contrario retorna un objeto Just [(String, String)] que representa
todas las preguntas que deben hacerse desde la raíz del oráculo con el valor de la opcion 
escogida
-}
--FALTA POR HACER 

--Intentando implementar BFS para poder extraer las cadenas

data BFSTreeNode = BFSTreeNode {
    predecesor :: Oraculo,
    value :: Oraculo
}

--       Cola       Visitados     Acumulador     Predecesor - Nodo
bfs :: [Oraculo] -> [Oraculo] -> [BFSTreeNode] ->  [BFSTreeNode]
bfs [] _ accum = accum --Si la cola está vacía retornar acumulador

--Caso en el que el siguiente elemento en la cola es una pregunta
bfs (OraculoPreg {pregunta = p, opciones = o} : q) seen accum = bfs queue seen' accum'
    where 
        --Se buscan los "nodos" adyacentes al elemento de la cola
        neighbors = [y | (_,y) <- Map.toList o]
        --Se encuentran qué nodos adyacentes no han sido visitados aun
        notVisitedNeighbors = filter (`notElem` seen) neighbors
        --Se agregan a la cola
        queue = q ++ filter (`notElem` q) notVisitedNeighbors
        --Se agrega el nodo actual a la lista de visitados
        seen' = seen ++ [OraculoPreg {pregunta = p, opciones = o}]
        --Para cada uno de los vecinos no visitados se marca el elemento actual como su predecesor
        accum' = accum ++ [BFSTreeNode {predecesor = OraculoPreg {pregunta = p, opciones = o}, value = y} | y <- notVisitedNeighbors]

bfs (OraculoPred {prediccion = p} : q) seen accum = bfs q seen' accum
    where
        --Se marca el nodo como visitado
        seen' = seen ++ [OraculoPred {prediccion = p}]