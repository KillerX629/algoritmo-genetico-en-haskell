import Data.List
import System.Random
import System.IO.Unsafe
import Control.Monad
import Data.Maybe
import Data.List
import Control.Monad


--definimos la estructura de individuo, que contendrá un valor flotante cómo gen y otro valor flotante como fitness
data Individuo = Individuo {
    gen :: Float,
    fitness :: Float} deriving (Eq)


--derivamos la clase Show para que nos muestre el gen y el fitness de un individuo
instance Show Individuo where
    show (Individuo gen fitness) = "Gen: " ++ show gen ++ " Fitness: " ++ show fitness ++"\n"


--creamos la funcion de mutación, que recibe un individuo y devuelve un individuo mutado
--para mutar al individuo, generamos un valor al azar entre -0.1 y 0.1 y le sumamos al gen del individuo el producto de ese numero por el gen del individuo
mutacion :: Individuo -> Individuo
mutacion individuo = Individuo (gen individuo + (unsafePerformIO (randomRIO (-0.1, 0.1::Float)))  * gen individuo) (objetivo (gen individuo))--recordamos que debemos reevaluar el fitness del individuo mutado

mutar :: [Individuo] -> [Individuo]
mutar individuos = if (length individuos == 1) then individuos else(if (unsafePerformIO(randomRIO (0,1::Float)) >= 0.5) then (mutacion (head individuos)) : (mutar (tail individuos)) else (head individuos) : (mutar (tail individuos)))

--creamos la funcion de cruce, que recibe dos individuos y devuelve un individuo cuyo gen es el promedio de los genes de los padres
cruce :: Individuo -> Individuo -> Individuo
cruce individuo1 individuo2 = Individuo ((gen individuo1 + gen individuo2)/2) (objetivo ((gen individuo1 + gen individuo2)/2) )--recordamos que debemos reevaluar el fitness del individuo cruzado
--cruzar([individuos])=[individuos]

cruzar ::   [Individuo]  -> [Individuo]--el 'a' nos permite que el compilador no intente inferir el tipo de la lista, dado que el compilador de haskell no permitiría usarlo
cruzar poblacion
  | length poblacion < 3 = generarIndividuos  1 --poblacion esta ordenada de mayor a menor fitness catMaybes :: [Maybe a] -> [a]
  | (unsafePerformIO(randomRIO( 0.0, 1.0 :: Float)))  > (1 / (fromIntegral(length poblacion)))=  (cruzar2 (head poblacion) (tail  poblacion) ++ (cruzar (tail poblacion)))
  | otherwise =  (cruzar (tail poblacion))


cruzar2 :: Individuo -> [Individuo] -> [Individuo] --convertimos el resultado en una lista
cruzar2 padre poblacion= singleton (cruce  padre(poblacion !! (unsafePerformIO(randomRIO (0 , ( length poblacion)-1 ::Int)))))


--creamos la funcion seleccion, que recibe una lista de individuos y devuelve una lista de individuos ordenados por fitness
seleccion :: [Individuo] -> [Individuo]
--seleccion x = sortBy (\x y -> compare (fitness x) (fitness y))
seleccion poblacion = sortBy (\x y -> compare (fitness x) (fitness y)) poblacion

--establecemos la función objetivo. en este caso será la función sen(x)*x
objetivo :: Float -> Float
objetivo x = sin x * x



--generamos una funcion que genere N individuos aleatorios, calculando la función objetivo para el gen generado
generarIndividuos :: Int -> [Individuo]
generarIndividuos n = map (\x -> Individuo (x) (objetivo x)) (map (\x -> unsafePerformIO (randomRIO (-100.0, 100.0::Float))) [1..n])


--generarIndividuos n = map (\x -> Individuo (unsafePerformIO (randomRIO (-10, 10::Float))) (objetivo (x gen))) n




--generamos una funcion que dadas dos listas de individuos, la primera de población original 
--y la segunda de los hijos, devuelve una sola lista donde los elementos de hijos hayan reemplazado a los peores elementos de la poblacion original
--NOTA: la lista de población original se encuentra ordenada en orden descendiente por fitness
reemplazar :: [Individuo] ->  [Individuo] -> [Individuo]
reemplazar poblacion hijos = take (div (length poblacion)2) ( hijos)  ++ take (div (length poblacion)2) (poblacion)

--creamos la función main, donde implementaremos el algoritmo genético
main :: IO ()
main = do
    let poblacion = generarIndividuos 10 --generamos la población inicial
    print(poblacion)
    -- ordenamos la poblacion inicial
    let poblacionOrdenada = seleccion poblacion
    print(poblacionOrdenada)
    print("----- CRUCE --------")
    let hijos = cruzar poblacion --generamos los hijos
    print(hijos)
    print("----- MUTACION --------")
    --mutamos a una parte de los hijos, en base a un número al azar
    let hijosMutados = mutar hijos
    print(hijosMutados)
    let hijosMutados12 = seleccion hijosMutados
    print("----- REEMPLAZO --------")
    let poblacion2 = (reemplazar poblacion hijosMutados12) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion2 <- return (seleccion poblacion2) --ordenamos la población
    print(poblacion2)
    let hijos2 = cruzar poblacion2
    print("----- CRUCE2 --------")
    print(hijos2)
    print("----- MUTACION2 --------")
    let hijosMutados21 = mutar hijos2
    print(hijosMutados21)
    let hijosMutados22 = seleccion hijosMutados21
    print("----- REEMPLAZO2 --------")
    let poblacion3 = (reemplazar poblacion2 hijosMutados22) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion3 <- return (seleccion poblacion3) --ordenamos la población
    print(poblacion3)
    let hijos3 = cruzar poblacion3
    print("----- CRUCE3 --------")
    print(hijos3)
    print("----- MUTACION3 --------")
    let hijosMutados31 = mutar hijos3
    print(hijosMutados31)
    let hijosMutados32 = seleccion hijosMutados31
    print("----- REEMPLAZO3 --------")
    let poblacion4 = (reemplazar poblacion3 hijosMutados32) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion4 <- return (seleccion poblacion4) --ordenamos la población
    print(poblacion4)
    let hijos4 = cruzar poblacion4
    print("----- CRUCE4 --------")
    print(hijos4)
    print("----- MUTACION4 --------")
    let hijosMutados41 = mutar hijos4
    print(hijosMutados41)
    let hijosMutados42 = seleccion hijosMutados41
    print("----- REEMPLAZO4 --------")
    let poblacion5 = (reemplazar poblacion4 hijosMutados42) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion5 <- return (seleccion poblacion5) --ordenamos la población
    print(poblacion5)
    --imprimimos el mejor individuo de cada generacion
    print ("------ MEJORES INDIVIDUOS ------")
    print("Generacion 0: ")
    print(head poblacion)
    print("Generacion 1: ")
    print(head poblacion2)
    print("Generacion 2: ")
    print(head poblacion3)
    print("Generacion 3: ")
    print(head poblacion4)
    print("Generacion 4: ")
    print(head poblacion5)
    print("------ FIN ------")


{-
    --ejecutamos el algoritmo N veces
    forM_ [1..10] (\x -> do
        print("----- ITERACION " ++ show x ++ " --------")
        let mutable poblacion = seleccion poblacion
        let mutable hijos = cruzar poblacion
        poblacion <- return (reemplazar poblacion hijos)
        poblacion <- return (seleccion poblacion)
        print (seleccion poblacion))
        --esperamos que el usuario presione enter para continuar
        --finalizamos el ciclo for
    
-}