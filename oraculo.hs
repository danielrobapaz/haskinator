module Oraculo
    (crearOraculo,
    respuesta,
    readOraculo,
    ramificar,
    obtenerEstadisticas,
    Oraculo(..),
    Opciones)
where

import qualified Data.Map as Map
import Data.Maybe
    
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

{-- FUNCIONES DE INSPECCION --}

{-
Recibe un oráculo. Devuelve una 3–tupla con los
siguientes datos:
El mínimo, máximo y promedio de preguntas que el
oráculo necesita hacer para llegar a alguna predicción.
-}
obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
obtenerEstadisticas o = case o of
    OraculoPred _ -> (min, max, avg)
        where
            min = minimum $ map (length . encontrarCamino) $ bfs [BFSTreeNode {predecesor = Nothing, value = o}] [] []
            max = maximum $ map (length . encontrarCamino) $ bfs [BFSTreeNode {predecesor = Nothing, value = o}] [] []
            avg = fromIntegral (sum $ map (length . encontrarCamino) $ bfs [BFSTreeNode {predecesor = Nothing, value = o}] [] []) / fromIntegral (length $ bfs [BFSTreeNode {predecesor = Nothing, value = o}] [] [])

    OraculoPreg _ _ -> (0, 0, 0.0)

{-
Recibe un oraculo y una cadena de caracteres que representa una predicción. Retorna un valor
de tipo Maybe [(String, String)]. Si la predicción suministrada no pertenece al oráculo 
retorna Nothing, en caso contrario retorna un objeto Just [(String, String)] que representa
todas las preguntas que deben hacerse desde la raíz del oráculo con el valor de la opcion 
escogida
-}
obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
obtenerCadena orac predict = do
    let bfsTree = bfs [BFSTreeNode {predecesor = Nothing, value = orac}] [] []
    let targetNodeSingleton = filter (`nodeIsPrediction` predict) bfsTree
    if null targetNodeSingleton then
        Nothing
    else
        do
            let pathToNode = encontrarCamino $ head targetNodeSingleton
            Just $ map (\(a, b) -> (pregunta a, fromJust $ obtenerClave b (opciones a))) pathToNode

nodeIsPrediction :: BFSTreeNode -> String -> Bool
nodeIsPrediction (BFSTreeNode {value = node}) s =
    case node of
        OraculoPred {prediccion = p} -> p == s
        OraculoPreg {pregunta = _, opciones = _} -> False

{-
Funcion que recibe un valor de un objeto Data.Map y retorna la clave 
que otorga el valor ingresado al evaluarla en el mapa. Si el valor ingresado
no existe en el mapa retorna Nothing.
-}
obtenerClave :: (Ord k, Eq v) => v -> Map.Map k v -> Maybe k
obtenerClave value = Map.foldrWithKey (\k v acc -> if v == value then Just k else acc) Nothing

{-
Funcion que recibe el nodo de un arbol BFS y retorna, en forma de pares
de oraculos, el camino desde el nodo ingresado hasta la raiz del arbol.
-}
encontrarCamino :: BFSTreeNode -> [(Oraculo, Oraculo)]
encontrarCamino predNode = makePath predNode [] where
    makePath :: BFSTreeNode -> [(Oraculo, Oraculo)] -> [(Oraculo, Oraculo)]
    makePath node path = if isNothing (predecesor node) then
        path
    else makePath (fromJust $ predecesor node) ((value $ fromJust (predecesor node), value node):path)

--Intentando implementar BFS para poder extraer las cadenas

data BFSTreeNode = BFSTreeNode {
    predecesor :: Maybe BFSTreeNode,
    value :: Oraculo
} deriving (Show)

instance Eq BFSTreeNode where
    (==) p q = predecesor p == predecesor q && value p == value q

--         Cola         Visitados     Acumulador     Predecesor - Nodo
bfs :: [BFSTreeNode] -> [Oraculo] -> [BFSTreeNode] ->  [BFSTreeNode]
bfs [] _ accum = accum --Si la cola está vacía retornar acumulador

--Caso en el que el siguiente elemento en la cola es una pregunta
bfs (BFSTreeNode {predecesor = pred, value = OraculoPreg {pregunta = preg, opciones = o}} : q) seen accum = bfs queue seen' accum'
    where 
        --Nodo actualmente bajo procesamiento
        current = Just BFSTreeNode {predecesor = pred, value = OraculoPreg {pregunta = preg, opciones = o}}
        --Se buscan los "nodos" adyacentes al elemento de la cola
        neighbors = [y | (_,y) <- Map.toList o]
        --Se encuentran qué nodos adyacentes no han sido visitados aun
        notVisitedNeighbors = filter (`notElem` seen) neighbors
        --Se agregan a la cola
        queue = q ++ filter (`notElem` q) [BFSTreeNode {predecesor = current, value = y} | y <- notVisitedNeighbors]
        --Se agrega el nodo actual a la lista de visitados
        seen' = seen ++ [OraculoPreg {pregunta = preg, opciones = o}]
        --Para cada uno de los vecinos no visitados se marca el elemento actual como su predecesor
        accum' = accum ++ [BFSTreeNode {predecesor = current, value = y} | y <- notVisitedNeighbors]

bfs (BFSTreeNode{value = OraculoPred {prediccion = p}} : q) seen accum = bfs q seen' accum
    where
        --Se marca el nodo como visitado
        seen' = seen ++ [OraculoPred {prediccion = p}]