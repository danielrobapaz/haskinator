import Oraculo
import System.IO
import System.Exit (exitSuccess)
import qualified Data.Map as Map

{-- FUNCIONES AUXILIARES DE TEXTO --}
regular :: IO ()
regular = do putStr "\ESC[1;0m"

bold :: IO ()
bold = do putStr "\ESC[1;1m"

italic :: IO ()
italic = do putStr "\ESC[3m"

yellow :: IO ()
yellow = do putStr "\ESC[33m"

haskinatorHabla :: IO ()
haskinatorHabla = do putStr "\ESC[1;3;34m"

{-- FUNCIONES DE CONSTRUCCIÓN --}

-- Pide al usuario el nombre de un archivo y crea
-- un nuevo oráculo con la información del mismo.
cargar :: IO Oraculo
cargar = do
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine

    contenido <- readFile nombreArchivo

    return $ readOraculo contenido

-- Pide al usuario el nombre de un archivo para
-- guardar el oráculo actual.
persistir :: Oraculo -> IO Oraculo
persistir oraculo = do
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine

    -- escribimos el archivo
    writeFile nombreArchivo (show oraculo)

    return oraculo


obtenerOraculos :: Opciones -> [Oraculo]
obtenerOraculos  = Map.elems

obtenerPredicciones :: Oraculo -> [String]
obtenerPredicciones (OraculoPred preg) = [preg]
obtenerPredicciones (OraculoPreg _ opc) = obtenerPrediccionesLista $ obtenerOraculos opc
    where
    obtenerPrediccionesLista :: [Oraculo] -> [String]
    obtenerPrediccionesLista [] = []
    obtenerPrediccionesLista (h:t) = obtenerPredicciones h ++ obtenerPrediccionesLista t

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (h:t) = (h `elem` t) || hayRepetidos t


{-
    Realiza el proceso de prediccion y devuelve el oraculo
    luego del proceso.

    Si durante el proceso se agerga al oraculo una prediccion repetida,
    se devuelve el oraculo original
-}
procesoPrediccion :: Oraculo -> IO Oraculo
procesoPrediccion (OraculoPred pred) = do 
    haskinatorHabla
    putStrLn $ "⋆⭒˚｡⋆ Adivina adivinador, la respuesta que estás buscando es: " ++ pred ++ "."
    putStrLn "⋆⭒˚｡⋆ Así que dime, ¿estoy en lo correcto? (SI|NO)"
    respuestaPrediccion <- getLine
    haskinatorHabla

    case respuestaPrediccion of 
        "SI" -> do 
            putStrLn "\n⋆⭒˚｡⋆ Por supuesto, ningún mortal es rival para mi.\n"
            return $ OraculoPred pred

        "NO" -> do
            putStrLn "\n⋆⭒˚｡⋆ Incluso un ser tan poderoso como yo puede tener momentos de debilidad..."
            
            putStrLn "⋆⭒˚｡⋆ A ver, joven humano. Dime la respuesta correcta"
            respuestaCorrecta <- getLine

            putStrLn "\n⋆⭒˚｡⋆ ¿Y cuál es la pregunta que la distingue de mi predicción?"
            preguntaDistingue <- getLine

            putStrLn "\n⋆⭒˚｡⋆ ¿Qué opción corresponde a la respuesta correcta?"
            opcionRespuestaIncorrecta <- getLine

            putStrLn "\n⋆⭒˚｡⋆ ¿Cuál es la opción que lleva a mi predicción? No pienso volver a equivocarme frente a un humano..."
            opcionRespuestaCorr <- getLine

            let opciones = [opcionRespuestaCorr, opcionRespuestaIncorrecta]
            let oraculos = [(OraculoPred pred), (OraculoPred respuestaCorrecta)]

            return $ ramificar opciones oraculos preguntaDistingue

        _ -> do 
            putStrLn "⋆⭒˚｡⋆ Las lenguas humanas no son mi fuerte. No pude entender qué dijiste.\n"
            return $ OraculoPred pred

procesoPrediccion (OraculoPreg preg opc) = do
    haskinatorHabla
    putStrLn preg
    putStrLn $ "\n⋆⭒˚｡⋆ Las posibles respuestas a tu pregunta son: " ++ show (Map.keys opc) ++ " o \'NINGUNA\'"
    putStrLn "\n⋆⭒˚｡⋆ Así que, humano, ¿cuál es tu respuesta?"
    respuesta <- getLine

    case respuesta of 
        "NINGUNA" -> do
            putStrLn "\n⋆⭒˚｡⋆ Cuales serían la opciones que esperarías?"
            nuevaOpc <- getLine

            putStrLn "\n ⋆⭒˚｡⋆ Cuál sería la respuesta correcta?"
            resp <- getLine

            let nuevasOpciones = Map.insert nuevaOpc (OraculoPred resp) opc
            let nuevoOraculo = OraculoPreg preg nuevasOpciones
            return $ nuevoOraculo

        _ -> do 
            let existeOpcion = Map.member respuesta opc

            case existeOpcion of 
                False -> do 
                    putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, podré ser viejo pero sé que esa opción no está entre las que te dí.\n"
                    return $ OraculoPreg preg opc

                True -> do
                    nuevoOraculo <- procesoPrediccion (opc Map.! respuesta)
                    let opcionesViejas = Map.delete respuesta opc
                    let opcionesNuevas = Map.insert respuesta nuevoOraculo opcionesViejas 
                    let nuevoOraculo = OraculoPreg preg opcionesNuevas
                    let prediccionesNuevas = obtenerPredicciones nuevoOraculo
                    
                    case hayRepetidos prediccionesNuevas of 
                        True -> do
                            putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, esa prediccion esta repetida.\n"
                            return $ OraculoPreg preg opc

                        False -> do
                            return $ nuevoOraculo
                    

{-- CLIENTE --}

-- Dependiendo de la opción escogida por el usuario,
-- se ejecuta la función correspondiente.
comenzarHaskinator :: Maybe Oraculo -> Char -> IO ()
comenzarHaskinator oraculo op = case op of

    '1' -> do -- crear oraculo
        putStrLn "Ingrese una predicción"
        prediccionUsuario <- getLine
        putStrLn "El nuevo oráculo ha sido creado.\n"
        preguntarOpcion (Just (crearOraculo prediccionUsuario))

    '2' -> do -- predecir
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                o <- procesoPrediccion o
                putStrLn $ show o
                preguntarOpcion $ Just o

    '3' -> do -- persistir
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                o <- persistir o
                putStrLn "Oraculo persistido exitosamente.\n"
                preguntarOpcion $ Just o

    '4' -> do -- cargar
        o <- cargar
        putStrLn "Oraculo cargado exitosamente.\n"
        preguntarOpcion $ Just o

    '5' -> do
        putStrLn "to do: consultar pregunta crucial"

    '6' -> do
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: estadísticas"

    '7' -> do -- salir
        haskinatorHabla
        putStrLn "⋆⭒˚｡⋆ Ten cuidado en tu regreso hacia la civilización, muchos misterios se esconden en el bosque."
        exitSuccess

    _ -> do -- opción inválida
        haskinatorHabla
        putStrLn "⋆⭒˚｡⋆  No puedo ayudarte, lo lamento."
        regular
        preguntarOpcion Nothing


verificarOpcion :: Char -> Bool
verificarOpcion s = s `elem` ['1', '2', '3', '4', '5', '6', '7']

-- Presenta el cliente y se encarga de pedir al usuario
-- que seleccione una de las opciones disponibles.
preguntarOpcion :: Maybe Oraculo -> IO ()
preguntarOpcion oraculo = do
    haskinatorHabla
    putStrLn "⋆⭒˚｡⋆ Joven mortal, ¿qué puedo hacer por ti? ⋆⭒˚｡⋆\n"
    regular
    italic
    putStrLn "Ingresa el número de la opción deseada."
    regular
    putStrLn "⋆ 1: Crear un nuevo oráculo"
    putStrLn "⋆ 2: Predecir"
    putStrLn "⋆ 3: Persistir"
    putStrLn "⋆ 4: Cargar"
    putStrLn "⋆ 5: Consultar pregunta crucial"
    putStrLn "⋆ 6: Estadísticas"
    putStrLn "⋆ 7: Salir"

    italic

    opcionEscogida <- getChar
    putStrLn "\n"

    if verificarOpcion opcionEscogida
        then comenzarHaskinator oraculo opcionEscogida
        else do
            haskinatorHabla
            putStrLn "⋆⭒˚｡⋆  Mi sabiduría es muy amplia, pero hay cosas que no puedo compartir a un simple humano.\n"-- >> main

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    italic
    putStrLn "En las profundidades del bosque de los mil y un monads,"
    putStrLn "en la apartada y escondida cuna del gran rio Curry,"
    putStrLn "vive en su choza de piedra el poderoso oraculo...\n"
    yellow
    putStrLn "✩₊˚.⋆☾⋆⁺₊✧✩₊˚.⋆☾⋆⁺₊✧✩₊˚.⋆☾⋆⁺₊✧\n"
    putStrLn "✩₊˚.⋆☾⋆⁺   HASKINATOR   .⋆☾⋆⁺₊✧\n"
    putStrLn "✩₊˚.⋆☾⋆⁺₊✧✩₊˚.⋆☾⋆⁺₊✧✩₊˚.⋆☾⋆⁺₊✧\n"
    regular
    preguntarOpcion Nothing
