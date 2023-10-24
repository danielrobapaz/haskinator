import Oraculo
import System.IO

{-- SERVIDOR --}

{-- CLIENTE --}
{--
comenzarHaskinator :: Maybe Oraculo -> Char -> IO ()
comenzarHaskinator oraculo op
    | op == 1 = putStrLn "Opción 1"
    | op == 2 = putStrLn "Opción 2"
    | op == 3 = putStrLn "Opción 3"
    | op == 4 = putStrLn "Opción 4"
    | op == 5 = putStrLn "Opción 5"
    | op == 6 = putStrLn "Opción 6"
    | op == 7 = putStrLn "Opción 7"
    | otherwise = putStrLn "Opción inválida"
--}

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

verificarOpcion :: String -> Bool
verificarOpcion s = s `elem` ["1", "2", "3", "4", "5", "6", "7"]

main :: IO ()
main = do
    putStrLn "Bienvenido a Haskinator!"

    putStrLn "Selecciona una de las siguientes opciones:\n"
    putStrLn "#1: Crear un nuevo oráculo"
    putStrLn "#2: Predecir"
    putStrLn "#3: Persistir"
    putStrLn "#4: Cargar"
    putStrLn "#5: Consultar pregunta crucial"
    putStrLn "#6: Estadísticas"
    putStrLn "#7: Salir"

    opcionEscogida <- getLine
    if verificarOpcion opcionEscogida
        then putStrLn "Opción válida"
        else putStrLn "Opción inválida.\n" -- >> main

    nombreArchivo <- getLine
    cargar nombreArchivo


    nombreArchivo <- getLine
    persistir nombreArchivo "holaaaa"


