# Haskinator - Proyecto I del Laboratorio de Lenguajes de Programacion (CI-3661)

# Tabla de contenido

1. [Introducción](#Introduccion)
2. [Oraculo.hs](#oraculohs)
    1. [Tipos de datos](#tipos-de-datos)
        1. [Oráculo](#oráculo)
        2. [Opciones](#opciones)
    2. [Estructuras de datos](#estructuras-de-datos)
    3. [Funciones internas](#funciones-internas)
3. [Haskinator.hs](#haskinatorhs)
4. [Ejecucion](#ejecución)
5. [Extras](#extras)

## Introducción

Este proyecto contempló la implementación de Haskinator, una adaptación de _Akinator_ o _Q10_ escrito en Haskell.

El código está distribuido en dos archivos, `Oraculo.hs` y `Haskinator.hs`. Se describen estos archivos en las siguientes líneas:

## [Oraculo.hs](Oraculo.hs)

Este archivo de Haskell contiene todas las estructuras de datos y funciones propias de _predicción_ de Haskinator.

### Tipos de datos

Se definieron los tipos de datos Oráculo y Opciones.

#### Oráculo

Un *oráculo* es la información que posee Haskinator, y puede ser de dos tipos: pregunta o predicción.

1. Tipo Pregunta: un oráculo tipo pregunta está conformado por una cadena de caracteres que corresponde a la pregunta y un diccionario de [Opciones](####Opciones) Opciones (tipo de datos) posibles para una pregunta.

Un oráculo pregunta tiene la siguiente sintaxis:

```haskell
OraculoPreg {
    pregunta = "String de la pregunta", 
    opciones = fromList [("Opción_0",OraculoPreg {...}),("Opción_1",OraculoPreg {...}),...,("Opción_k", OraculoPred {...} )]
}
```

El diccionario de opciones es una tupla de opciones, donde la clave es la opción y el valor un oráculo pregunta o predicción.

2. Tipo Predicción: este oráculo viene siendo una cadena de caracteres, y representa la respuestá final a todas las preguntas.

La sintaxis de un oráculo predicción es más sencilla, únicamente tiene el string de la predicción.

```haskell
OraculoPred {prediccion = "String de la predicción"}
``` 
#### Opciones

El tipo de datos **opciones** es una tupla que contiene el string de la respuesta y un oráculo pregunta o predicción.

### Estructuras de datos

La estructura de datos elemental que funciona como base fundamental del proyecto es el árbol, cuyos nodos corresponden a los valores del oraculo (pregunta o predicción), y los arcos corresponden con las opciones.

El árbol definido por un oráculo está implícitamente definido, por lo cual se creó un tipo de datos auxiliar en pro de facilitar la navegación por el mismo. Esta estructura de datos es el data `BFSTreeNode` definido
en Oraculo.hs. 

El árbol es recorrido usando el algoritmo de BFS, definido en una función homónima.

### Funciones internas

Dentro de `Oráculo.hs` se definieron las funciones internasa solicitadas en el enunciado del proyecto, entre las cuales están:

- Funciones de construcción: crear oráculo y ramificar
- Funciones de acceso: predicción, pregunta, opciones, respuesta
- Funciones de inspección: obtenerCadena y obtenerEstadísticas
- Instancias: clases Show y Read.

Además están las funciones auxiliares:

- preguntaCrucial, nodeIsPrediction, obtenerClave, obtenerLargoPorPrediccion, obtenerLargoCadena, obtenerPredicciones, encontrarCamino y bfs.


## [Haskinator.hs](Haskinator.hs)

Este es el archivo que contiene al _cliente_ de Haskinator, es decir, es la parte con la que interactúa el usuario. 

Dentro de `Haskinator.hs` se definieron las siguientes funciones:

- Main: en esta se incluyeron persistir, obtenerOraculos, haskinatorHabla, obtenerPredicciones, hayRepetidos, procesoPrediccion, comenzarHaskinator,verificarOpcion, preguntarOpcion.

Además, el cliente se ejecuta y siempre está a la espera de las solicitudes del usuario Estas son:

1. Crear oráculo
2. Predecir
3. Persistir
4. Cargar
5. Consultar Pregunta Crucial
6. Estadísticas
7. Salir

Cada opción se ejecuta al presionar el número correspondiente con la opción.

## Ejecución

Para la ejecución de `Haskinator` se debe tener instalado el compilador `GHC`. Los pasos son sencillos:

```bash
$ make 
$ ./Haskinator
```

Al ejecutarlo, no existirá un oráculo. Se requiere crear uno con la opción 1 o cargarlo con la opción 4.

## Extras y detalles a considerar

- Para la representación de Strings, mantuvimos la representación de Show sin realizar una definición propia.
- El cliente Haskinator no es capaz de leer archivos de manera _segura_, de esta forma, si el archivo proveído al solicitar la función Cargar no existe o está mal formateado, el programa culminará en excepción sin forma de recuperarse
- De la misma forma, si se intenta persistir un oráculo en un archivo en un directorio que no existe, el programa también crasheará.
- La lectura de los inputs es case-sensitive. De esta forma, si se tienen las opciones "SI" y "NO"; "si", "Si" y "nO" no serán interpretadas como respuestas válidas.
