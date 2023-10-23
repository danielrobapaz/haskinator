import qualified Data.Map as Map


type Opciones = Map.Map String Oraculo

data Oraculo = OraculoPred { prediccion :: String}
             | OraculoPreg { pregunta :: String, opciones :: Opciones}
             deriving (Show, Read)


respuesta :: Oraculo -> String -> Oraculo
respuesta (OraculoPred _) _ = error "No se puede obtener respuesta de una prediccion"
respuesta (OraculoPreg _ o) s = o Map.! s

{--
obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
obtenerCadena (OraculoPred p) s = 
obtenerCadena (OraculoPreg p o) s =  
--}