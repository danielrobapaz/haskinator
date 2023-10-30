module Haskinator (
    main
) where

import Oraculo
import System.IO
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Exit (exitSuccess)
import qualified Data.Map as Map
import Data.Maybe

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

-- Pide al usuario el nombre de un archivo para
-- guardar el oráculo actual.
persistir :: Oraculo -> IO Oraculo
persistir oraculo = do
    putStr "Ingrese el nombre del archivo: "
    hFlush stdout
    nombreArchivo <- getLine
    -- escribimos el archivo
    writeFile nombreArchivo (show oraculo)
    return oraculo

-- dado el diccionario de opciones de un oraculo
-- devuelve los oraculos correspondientes
obtenerOraculos :: Opciones -> [Oraculo]
obtenerOraculos  = Map.elems

-- dado un oraculo
-- devuelve todas sus prediccion
obtenerPredicciones :: Oraculo -> [String]
obtenerPredicciones (OraculoPred preg) = [preg]
obtenerPredicciones (OraculoPreg _ opc) = obtenerPrediccionesLista $ obtenerOraculos opc
    where
    obtenerPrediccionesLista :: [Oraculo] -> [String]
    obtenerPrediccionesLista [] = []
    obtenerPrediccionesLista (h:t) = obtenerPredicciones h ++ obtenerPrediccionesLista t

-- dada una lista de strings
-- devuelve true si hay elementos repetidos
--          false caso contrario
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
    putStrLn $ "\n⋆⭒˚｡⋆ Adivina adivinador, la respuesta que estás buscando es: " ++ pred ++ "."
    putStr "⋆⭒˚｡⋆ Así que dime, ¿estoy en lo correcto? (SI|NO): "
    hFlush stdout
    respuestaPrediccion <- getLine
    haskinatorHabla

    case respuestaPrediccion of 
        "SI" -> do 
            putStrLn "\n⋆⭒˚｡⋆ Por supuesto, ningún mortal es rival para mi.\n"
            return $ OraculoPred pred

        "NO" -> do
            putStrLn "\n⋆⭒˚｡⋆ Incluso un ser tan poderoso como yo puede tener momentos de debilidad..."
            
            putStr "⋆⭒˚｡⋆ A ver, joven humano. Dime la respuesta correcta: "
            hFlush stdout
            respuestaCorrecta <- getLine

            putStr "\n⋆⭒˚｡⋆ ¿Y cuál es la pregunta que la distingue de mi predicción?: "
            hFlush stdout
            preguntaDistingue <- getLine

            putStr "\n⋆⭒˚｡⋆ ¿Qué opción corresponde a la respuesta correcta?: "
            hFlush stdout
            opcionRespuestaIncorrecta <- getLine

            putStr "\n⋆⭒˚｡⋆ ¿Cuál es la opción que lleva a mi predicción? No pienso volver a equivocarme frente a un humano...: "
            hFlush stdout
            opcionRespuestaCorr <- getLine

            let opciones = [opcionRespuestaCorr, opcionRespuestaIncorrecta]
            let oraculos = [OraculoPred pred, OraculoPred respuestaCorrecta]

            return $ ramificar opciones oraculos preguntaDistingue

        _ -> do 
            putStrLn "⋆⭒˚｡⋆ Las lenguas humanas no son mi fuerte. No pude entender qué dijiste.\n"
            return $ OraculoPred pred

procesoPrediccion (OraculoPreg preg opc) = do
    haskinatorHabla
    putStrLn $ "\n⋆⭒˚｡⋆ Pregunto: " ++ preg
    putStrLn $ "⋆⭒˚｡⋆ Las posibles respuestas a tu pregunta son: " ++ show (Map.keys opc) ++ " o \'NINGUNA\'"
    putStr "⋆⭒˚｡⋆ Así que, humano, ¿cuál es tu respuesta?: "
    hFlush stdout
    respuesta <- getLine

    case respuesta of 
        "NINGUNA" -> do
            putStr "\n⋆⭒˚｡⋆ Cuales sería la opción que esperarías?: "
            hFlush stdout
            nuevaOpc <- getLine

            putStr "\n ⋆⭒˚｡⋆ Cuál sería la respuesta correcta?: "
            hFlush stdout
            resp <- getLine

            let nuevasOpciones = Map.insert nuevaOpc (OraculoPred resp) opc
            let nuevoOraculo = OraculoPreg preg nuevasOpciones
            let prediccionesNuevas = obtenerPredicciones nuevoOraculo
            if hayRepetidos prediccionesNuevas then
                do
                    putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, esa prediccion esta repetida.\n"
                    return $ OraculoPreg preg opc
                else 
                    return $ nuevoOraculo

        _ -> do 
            let existeOpcion = Map.member respuesta opc

            if existeOpcion then
                do
                    nuevoOraculo <- procesoPrediccion (opc Map.! respuesta)
                    let opcionesViejas = Map.delete respuesta opc
                    let opcionesNuevas = Map.insert respuesta nuevoOraculo opcionesViejas 
                    let nuevoOraculo = OraculoPreg preg opcionesNuevas
                    let prediccionesNuevas = obtenerPredicciones nuevoOraculo
                    
                    if hayRepetidos prediccionesNuevas then
                        do
                            putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, esa prediccion esta repetida.\n"
                            return $ OraculoPreg preg opc
                        else
                            return $ nuevoOraculo
                            
            else
                do
                    putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, podré ser viejo pero sé que esa opción no está entre las que te dí.\n"
                    return $ OraculoPreg preg opc
                    
{-- CLIENTE --}

-- Dependiendo de la opción escogida por el usuario,
-- se ejecuta la función correspondiente.
comenzarHaskinator :: Maybe Oraculo -> String -> IO ()
comenzarHaskinator oraculo op = case op of

    "1" -> do -- crear oraculo
        putStrLn "Ingrese una predicción"
        prediccionUsuario <- getLine
        putStrLn "El nuevo oráculo ha sido creado.\n"
        preguntarOpcion (Just (crearOraculo prediccionUsuario))

    "2" -> do -- predecir
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                haskinatorHabla
                putStrLn "\n⋆⭒˚｡⋆ De acuerdo... Empecemos a jugar humano ;)\n"
                o <- procesoPrediccion o
                preguntarOpcion $ Just o

    "3" -> do -- persistir
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                o <- persistir o
                putStrLn "Oraculo persistido exitosamente.\n"
                preguntarOpcion $ Just o

    "4" -> do -- cargar
        putStr "Ingrese el nombre del archivo: "
        hFlush stdout
        nombreArchivo <- getLine
        contenido <- readFile nombreArchivo
        let nuevoOraculo = readOraculo contenido
        case hayRepetidos $ obtenerPredicciones nuevoOraculo of
            True -> do 
                putStrLn "\n⋆⭒˚｡⋆ No intentes vacilarme, hay predicciones repetidas.\n"
                preguntarOpcion $ Nothing
            False -> do
                putStrLn "Oraculo cargado exitosamente" 
                preguntarOpcion $ Just nuevoOraculo

    "5" -> do
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                putStr "¿Para cuáles dos predicciones te gustaría hallar la pregunta crucial?: "
                hFlush stdout
                predictionsInput <- getLine
                let predictions = words predictionsInput
                if length predictions /= 2 then
                    do
                        putStrLn "No, no. Con calma humano, vayamos de dos predicciones a la vez ;)"
                        preguntarOpcion $ Just o
                else
                    do
                        let crucialQuestion = preguntaCrucial o (head predictions) (last predictions)
                        case crucialQuestion of
                            Nothing -> do
                                haskinatorHabla
                                putStrLn "Hmmm... Parece que alguna de las predicciones que ingresaste no existen. ¿Tal vez intenta de nuevo?\n"
                                preguntarOpcion $ Just o
                            Just question -> do
                                haskinatorHabla
                                putStrLn "La pregunta que separa tu destino entre llegar a: "
                                yellow
                                putStrLn $ "\t" ++ show (head predictions)
                                haskinatorHabla
                                putStrLn "o a:"
                                yellow
                                putStrLn $ "\t" ++ show (last predictions)
                                haskinatorHabla
                                putStrLn "es:"
                                yellow
                                putStrLn $ "\t" ++ show question
                                putStrLn "\n"
                                preguntarOpcion $ Just o


    "6" -> do
        case oraculo of
            Nothing -> do
                putStrLn "Haskinator es muy sabio, pero sin un oráculo cargado no te puede ayudar.\n"
                preguntarOpcion Nothing
            Just o -> do
                let (min, max, avg) = obtenerEstadisticas o
                haskinatorHabla
                putStrLn "⋆⭒˚｡⋆ Efectivamente... Este oráculo es muy interesante. Te diré algunas cosas sobre él...\n"
                putStrLn "Longitud de la rama de predicción más corta:"
                yellow
                putStrLn $ "\t" ++ show min
                haskinatorHabla
                putStrLn "Longitud de la rama de predicción más larga:"
                yellow
                putStrLn $ "\t" ++ show max
                haskinatorHabla
                putStrLn "Longitud promedio de ramas de predicción:"
                yellow
                putStrLn $ "\t" ++ show avg ++ "\n"

                preguntarOpcion $ Just o

    "7" -> do -- salir
        haskinatorHabla
        putStrLn "⋆⭒˚｡⋆ Ten cuidado en tu regreso hacia la civilización, muchos misterios se esconden en el bosque."
        regular
        exitSuccess

    _ -> do -- opción inválida
        haskinatorHabla
        putStrLn "⋆⭒˚｡⋆  No puedo ayudarte, lo lamento."
        regular
        preguntarOpcion Nothing


verificarOpcion :: String -> Bool
verificarOpcion s = s `elem` ["1", "2", "3", "4", "5", "6", "7"]

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

    putStr "Opción a escoger: "
    hFlush stdout
    opcionEscogida <- getLine

    if verificarOpcion opcionEscogida
        then comenzarHaskinator oraculo opcionEscogida
        else do
            haskinatorHabla
            putStrLn "⋆⭒˚｡⋆  Mi sabiduría es muy amplia, pero hay cosas que no puedo compartir a un simple humano.\n"-- >> main

main :: IO ()
main = do
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
