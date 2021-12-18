import Data.List
import System.Random
import System.IO.Unsafe
import Control.Monad
import Data.Maybe
import Data.List



--definimos la estructura de individuo, que contendrá un valor flotante cómo gen y otro valor flotante como fitness
data Individuo = Individuo {
    gen :: Float,
    fitness :: Float} deriving (Show, Eq)


--creamos la funcion de mutación, que recibe un individuo y devuelve un individuo mutado
--para mutar al individuo, generamos un valor al azar entre -0.1 y 0.1 y le sumamos al gen del individuo el producto de ese numero por el gen del individuo
mutacion :: Individuo -> Individuo
mutacion individuo = Individuo (gen individuo + (unsafePerformIO (randomRIO (-0.1, 0.1::Float)))  * gen individuo) (objetivo (gen individuo))--recordamos que debemos reevaluar el fitness del individuo mutado

--creamos la funcion de cruce, que recibe dos individuos y devuelve un individuo cuyo gen es el promedio de los genes de los padres
cruce :: Individuo -> Individuo -> Individuo
cruce individuo1 individuo2 = Individuo ((gen individuo1 + gen individuo2)/2) (objetivo ((gen individuo1 + gen individuo2)/2) )--recordamos que debemos reevaluar el fitness del individuo cruzado
--cruzar([individuos])=[individuos]

cruzar ::   [Individuo]  -> [Individuo]--el 'a' nos permite que el compilador no intente inferir el tipo de la lista, dado que el compilador de haskell no permitiría usarlo
cruzar poblacion
  | length poblacion < 3 = generarIndividuos  1 --poblacion esta ordenada de mayor a menor fitness catMaybes :: [Maybe a] -> [a]
  | uniformR(0::Float , 1::Float) > div 1 (round(length poblacion)) =  (cruzar2 (head poblacion) (tail  poblacion) ++ (cruzar (tail poblacion)))
  | otherwise =  (cruzar (tail poblacion))


test  :: Int -> Float
test a = a


cruzar2 :: Individuo -> [Individuo] -> [Individuo] --convertimos el resultado en una lista
cruzar2 padre [poblacion]= singleton (cruce (poblacion !! (randomR (0::Int , (length poblacion)-1 ::Int)) padre))



--creamos la funcion seleccion, que recibe una lista de individuos y devuelve una lista de individuos ordenados por fitness
seleccion :: [Individuo] -> [Individuo]
--seleccion x = sortBy (\x y -> compare (fitness x) (fitness y))
seleccion poblacion = sortBy (\x y -> compare (fitness x) (fitness y)) poblacion

--establecemos la función objetivo. en este caso será la función sen(x)*x
objetivo :: Float -> Float
objetivo x = sin x * x



--generamos una funcion que genere N individuos aleatorios
generarIndividuos :: Int -> [Individuo]
generarIndividuos n = map (\x -> Individuo (unsafePerformIO (randomRIO (-10, 10::Float))) (objetivo (x gen))) [1..(round n)]




--generamos una funcion que dadas dos listas de individuos, la primera de población original 
--y la segunda de los hijos, devuelve una sola lista donde los elementos de hijos hayan reemplazado a los peores elementos de la poblacion original
--NOTA: la lista de población original se encuentra ordenada en orden descendiente por fitness
reemplazar :: [Individuo] ->  [Individuo] -> [Individuo]
reemplazar poblacion hijos = take (length poblacion) ( hijos ++ poblacion)

--creamos la función main, donde implementaremos el algoritmo genético
main :: IO ()
main = do

    let poblacion = generarIndividuos 10 --generamos la población inicial
    let poblacion = seleccion poblacion --ordenamos la población inicial
    let hijos = cruzar poblacion --generamos los hijos
    poblacion <- return (reemplazar poblacion hijos) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion <- return (seleccion poblacion) --ordenamos la población
    print poblacion --imprimimos la población final