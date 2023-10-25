import Oraculo
import System.IO
import System.Exit (exitSuccess)

{-- FUNCIONES AUXILIARES --}
procesoPrediccion :: Oraculo -> IO ()
procesoPrediccion oraculo = case oraculo of
    OraculoPred pred -> do 
        putStrLn $ "Mi prediccion es: " ++ pred ++ "!!!"
        putStrLn "Es correcta mi prediccion? (SI|NO)"
        respuestaPrediccion <- getLine

        case respuestaPrediccion of 
            "SI" -> do 
                putStrLn "He acertado tu prediccion :D"

            "NO" -> do
                putStrLn "Dime la respuesta correcta: "
                respuestaCorrecta <- getLine

                putStrLn "Dime la pregunta que la distinga de la prediccion: "
                preguntaDistingue <- getLine

                putStrLn "Dime la opcion que corresponde a la respuesta equivocada: "

    OraculoPreg preg opc ->
        putStrLn $ "Mi pregunta es: " ++ preg


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
                putStrLn "to do: predecir"
                procesoPrediccion o
                preguntarOpcion $ Just o

    '3' -> do -- persistir
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "Ingrese el nombre del archivo: "
                nombreArchivo <- getLine

                 -- escribimos el archivo
                writeFile nombreArchivo (show o)

                preguntarOpcion $ Just o

    '4' -> do -- cargar
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: cargar"

    '5' -> putStrLn "to do: consultar pregunta crucial"

    '6' -> do
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: estadísticas"

    '7' -> do
        putStrLn "¡Hasta Luego!"
        exitSuccess

    _ -> do 
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
