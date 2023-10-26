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
procesoPrediccion :: Oraculo -> IO Oraculo
procesoPrediccion (OraculoPred pred) = do 
    putStrLn $ "Mi prediccion es: " ++ pred ++ "!!!"
    putStrLn "Es correcta mi prediccion? (SI|NO)"
    respuestaPrediccion <- getLine

    case respuestaPrediccion of 
        "SI" -> do 
            putStrLn "Lo he logrado :DDDD."
            return $ OraculoPred pred

        "NO" -> do
            putStrLn "Oh, no DDDD:"
            
            putStrLn "Dime la respuesta correcta: "
            respuestaCorrecta <- getLine

            putStrLn "Dime la pregunta que la distinga de la prediccion: "
            preguntaDistingue <- getLine

            putStrLn "Dime la opcion que corresponde a la respuesta correcta: "
            opcionRespuestaIncorrecta <- getLine

            putStrLn "Dime la opcion que lleva a mi prediccion: "
            opcionRespuestaCorr <- getLine

            let opciones = [opcionRespuestaCorr, opcionRespuestaIncorrecta]
            let oraculos = [(OraculoPred pred), (OraculoPred respuestaCorrecta)]

            return $ ramificar opciones oraculos preguntaDistingue

        _ -> do 
            putStrLn "Opcion equivocada :((("
            return $ OraculoPred pred

procesoPrediccion (OraculoPreg preg opc) = do
    putStrLn preg
    putStrLn $ "Las opciones son: " ++ show (Map.keys opc) ++ " o \'NINGUNA\'"
    putStrLn "Tu respuesta es: "
    respuesta <- getLine

    case respuesta of 
        "NINGUNA" -> do
            putStrLn "Cual seria la opciones que esperarias?"
            nuevaOpc <- getLine

            putStrLn "Cual seria la respuesta correcta?"
            resp <- getLine

            let nuevasOpciones = Map.insert nuevaOpc (OraculoPred resp) opc
            let nuevoOraculo = OraculoPreg preg nuevasOpciones
            return $ nuevoOraculo

        _ -> do 
            let existeOpcion = Map.member respuesta opc

            case existeOpcion of 
                False -> do 
                    putStrLn "Esa opcion no existe :(("
                    return $ OraculoPreg preg opc

                True -> do
                    nuevoOraculo <- procesoPrediccion (opc Map.! respuesta)
                    let opcionesViejas = Map.delete respuesta opc
                    let opcionesNuevas = Map.insert respuesta nuevoOraculo opcionesViejas 
                    return $ OraculoPreg preg opcionesNuevas
                    

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
                o <- procesoPrediccion o
                putStrLn $ show o
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
