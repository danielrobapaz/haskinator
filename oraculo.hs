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
crearOraculo = OraculoPred

-- recibe una lista de strings, una de oráculos y un string,
-- devuelve un oráculo del tipo pregunta con el string y las opciones
ramificar :: [String] -> [Oraculo] -> String -> Oraculo