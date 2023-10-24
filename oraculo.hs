module Oraculo
    (crearOraculo,
    respuesta,
    Oraculo(..))
where

import qualified Data.Map as Map
    
{-- TIPOS DE DATOS --}

type Opciones = Map.Map String Oraculo

data Oraculo = OraculoPred { prediccion :: String}
             | OraculoPreg { pregunta :: String, opciones :: Opciones}
             deriving (Show, Read)

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
crearOraculo s 
    | last s == '?' = OraculoPreg {pregunta = s, opciones = Map.empty}
    | otherwise = OraculoPred {prediccion = s}

-- recibe una lista de strings, una de oráculos y un string,
-- devuelve un oráculo del tipo pregunta con el string y las opciones
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar sl ol s = OraculoPreg {pregunta = s, opciones = Map.fromList $ zip sl ol}

{-
Recibe un oraculo y una cadena de caracteres que representa una predicción. Retorna un valor
de tipo Maybe [(String, String)]. Si la predicción suministrada no pertenece al oráculo 
retorna Nothing, en caso contrario retorna un objeto Just [(String, String)] que representa
todas las preguntas que deben hacerse desde la raíz del oráculo con el valor de la opcion 
escogida
-}

obtenerCadenaAux :: Oraculo -> String -> [(String, String)] -> Maybe [(String, String)]
obtenerCadenaAux (OraculoPreg {pregunta = p, opciones = o}) s l
    | any (\(x, OraculoPred {prediccion = y}) -> y == s) (Map.toList o) = Just $ l ++ [(p, s)] --Encontrada la predicción
    | otherwise = Nothing --Si la predicción no está entre las opciones (FALTA HACER)