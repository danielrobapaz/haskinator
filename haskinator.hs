import Oraculo
import System.IO
import System.Exit (exitSuccess)

{-- FUNCIONES AUXILIARES --}

cargar :: String -> IO ()
cargar s = do
    handle <- openFile s ReadMode
    contents <- hGetContents handle
    -- aqui iria algo bien mamalon
    putStrLn contents
    hClose handle

-- la idea es que reciba un oraculo jeje
persistir :: String -> String -> IO()
persistir f s = 
    do 
        appendFile f s -- aqui iria algo bien mamalon

{-- CLIENTE --}

-- Dependiendo de la opción escogida por el usuario,
-- se ejecuta la función correspondiente.
comenzarHaskinator :: Maybe Oraculo -> Char -> IO ()
comenzarHaskinator oraculo op = case op of
    '1' -> do
        putStrLn "Ingrese una prediccion: "
        prediccionUsuario <- getLine

        preguntarOpcion (Just (crearOraculo prediccionUsuario))
    '2' -> do
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: predecir"

    '3' -> do
        case oraculo of
            Nothing -> do
                putStrLn "No hay oráculo cargado.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStrLn "to do: persistir"

    '4' -> do
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

    -- en las opciones 3 y 4, se piden los nombres de los archivos
    -- eso es dentro de las funciones tho, aquí no
    
    {--nombreArchivo <- getLine
    cargar nombreArchivo

    nombreArchivo <- getLine
    persistir nombreArchivo "holaaaa"--}

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
    putStrLn "Bienvenido a Haskinator!"
    preguntarOpcion Nothing
