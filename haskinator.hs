import Oraculo
import System.IO
import System.Exit (exitSuccess)
import qualified Data.Map as Map

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

{-- FUNCIONES AUXILIARES --}

predecir :: Oraculo -> IO Oraculo
predecir oraculo = case oraculo of
    -- Llegamos a una predicción:
    OraculoPred pred -> do 
        putStrLn ("Prediccion: " ++ pred)
        putStrLn "Si / No"
        respuestaPrediccion <- getLine

        case respuestaPrediccion of 
            "Si" -> do 
                putStrLn "He acertado tu prediccion :D"
                return oraculo

            "No" -> do
                putStrLn "¡He fallado! ¿Cuál era la respuesta correcta?"
                respuestaCorrecta <- getLine

                putStrLn ("¿Qué pregunta distingue a " ++ respuestaCorrecta ++ " de las otras opciones?")
                preguntaDistingue <- getLine

                putStrLn ("¿Cuál es la respuesta a \"" ++ preguntaDistingue ++ "\" para" ++ respuestaCorrecta ++ "?")
                opcionRespuestaCorr <- getLine

                putStrLn ("¿Cuál es la respuesta a \"" ++ preguntaDistingue ++ "\" para" ++ pred ++ "?")
                opcionRespuestaIncorrecta <- getLine

                let predIncorrecta = OraculoPred pred
                let predCorrecta = OraculoPred respuestaCorrecta
                --let oraculoNuevo = OraculoPreg preguntaDistingue (Map.fromList [(opcionRespuestaCorr, predCorrecta), (opcionRespuestaIncorrecta, predIncorrecta)])

                let oraculoNuevo = ramificar [opcionRespuestaIncorrecta, opcionRespuestaCorr] [predIncorrecta, predCorrecta] preguntaDistingue
                return oraculoNuevo

            _ -> do
                putStrLn "Opcion inválida."
                return oraculo

    -- LLegamos a una pregunta: 
    OraculoPreg preg opc -> do
        putStrLn $ "Mi pregunta es: " ++ preg
        putStrLn $ "Las opciones son: " ++ show (Map.keys opc) ++ " o \'NINGUNA\'"
        putStrLn "Tu respuesta es: "

        respuesta <- getLine

        case respuesta of
            s -> do 
                if Map.member s opc
                then do
                    predecir (opc Map.! s)
                else do
                    putStrLn "Opción inválida. Hasta luego"
                    exitSuccess

            -- En caso de no ser ninguna de las opciones, se agrega la
            -- nueva opción a la lista anterior (opc) y tambien la nueva
            -- prediccion.
            "NINGUNA" -> do
                putStrLn "¿Qué opción esperabas"
                opcion <- getLine

                putStrLn "¿Cuál es la respuesta correcta?"
                respuesta <- getLine

                let nuevaPrediccion = OraculoPred respuesta
                    posiblesOpciones = Map.toList opc ++ [(opcion, nuevaPrediccion)]
                    predicciones = 
                    nuevoOraculo = ramificar (posiblesOpciones) (predicciones) respuesta

                return nuevoOraculo
            
{-- CLIENTE --}

-- Dependiendo de la opción escogida por el usuario,
-- se ejecuta la función correspondiente.
comenzarHaskinator :: Maybe Oraculo -> Char -> IO ()
comenzarHaskinator oraculo op = case op of
    '1' -> do -- crear oraculo
        putStrLn "Ingrese una prediccion: "
        prediccionUsuario <- getLine

        preguntarOpcion (Just (crearOraculo prediccionUsuario))

    '2' -> do -- predecir
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                --procesoPrediccion o
                predecir o
                preguntarOpcion $ Just o

    '3' -> do -- persistir
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
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
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: estadísticas"

    '7' -> do -- salir
        putStrLn "¡Hasta Luego!"
        exitSuccess

    _ -> do -- opción inválida
        putStrLn "Opción inválida"
        preguntarOpcion Nothing


verificarOpcion :: Char -> Bool
verificarOpcion s = s `elem` ['1', '2', '3', '4', '5', '6', '7']

-- Presenta el cliente y se encarga de pedir al usuario
-- que seleccione una de las opciones disponibles.
preguntarOpcion :: Maybe Oraculo -> IO ()
preguntarOpcion oraculo = do
    putStrLn "Selecciona una de las siguientes opciones:\n"
    putStrLn "#1: Crear un nuevo oráculo"
    putStrLn "#2: Predecir"
    putStrLn "#3: Persistir"
    putStrLn "#4: Cargar"
    putStrLn "#5: Consultar pregunta crucial"
    putStrLn "#6: Estadísticas"
    putStrLn "#7: Salir"

    opcionEscogida <- getChar
    putStrLn "\n"

    if verificarOpcion opcionEscogida
        then comenzarHaskinator oraculo opcionEscogida
        else putStrLn "Opción inválida.\n" -- >> main

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "Bienvenido a Haskinator!"
    preguntarOpcion Nothing
