{-En este archivo, crearemos las funciones necesarias para 
recorrer el dominio de sen(x)*x y buscar el máximo absoluto 
con un algoritmo genético, implementado en el paradigma funcional.-}



import System.randomRIO
 
--definimos la estructura de individuo, que contendrá un valor flotante cómo gen y otro valor flotante como fitness
data Individuo = Individuo {
    gen :: Float,
    fitness :: Float} deriving (Show, Eq)


--creamos la funcion de mutación, que recibe un individuo y devuelve un individuo mutado
--para mutar al individuo, generamos un valor al azar entre -0.1 y 0.1 y le sumamos al gen del individuo el producto de ese numero por el gen del individuo
mutacion :: Individuo -> Individuo
mutacion individuo = Individuo (gen individuo + randomRIO (-0.1, 0.1) * gen individuo) (objetivo gen Individuo)--recordamos que debemos reevaluar el fitness del individuo mutado

--creamos la funcion de cruce, que recibe dos individuos y devuelve un individuo cuyo gen es el promedio de los genes de los padres
cruce :: Individuo -> Individuo -> Individuo
cruce individuo1 individuo2 = Individuo ((gen individuo1 + gen individuo2)/2) (objetivo gen Individuo)--recordamos que debemos reevaluar el fitness del individuo cruzado


cruzar :: [Individuo]  -> Maybe [Individuo]
cruzar poblacion
  | length poblacion < 2 = Nothing
  | randomRIO(0,1) > 1/length poblacion = Just cruzar2 head poblacion tail poblacion ++ cruzar tail poblacion
  | otherwise = Just cruzar tail poblacion

cruzar2 :: individuo -> [individuo] -> individuo
cruzar2 padre [poblacion]=cruce poblacion !! randomIO(0,length poblacion /2)  padre


--creamos la funcion seleccion, que recibe una lista de individuos y devuelve una lista de individuos ordenados por fitness
seleccion :: [Individuo] -> [Individuo]
seleccion = sortBy (\x y -> compare (fitness x) (fitness y))

--establecemos la función objetivo. en este caso será la función sen(x)*x
objetivo :: Float -> Float
objetivo x = sin x * x

--generamos una funcion que genere N individuos aleatorios
generarIndividuos :: Int -> [Individuo]
generarIndividuos n = replicate n ((Individuo (randomRIO (-10, 10)) . objetivo . gen) (Individuo 0 0))

--generamos una funcion que dadas dos listas de individuos, la primera de población original 
--y la segunda de los hijos, devuelve una sola lista donde los elementos de hijos hayan reemplazado a los peores elementos de la poblacion original
--NOTA: la lista de población original se encuentra ordenada en orden descendiente por fitness
reemplazar :: [Individuo] -> [Individuo] -> [Individuo]
reemplazar poblacion hijos = take (length poblacion) (hijos ++ poblacion)

--creamos la función main, donde implementaremos el algoritmo genético
main = do
    let poblacion = generarIndividuos 10 --generamos la población inicial
    let poblacion = seleccion poblacion --ordenamos la población inicial
    let hijos = cruzar poblacion --generamos los hijos
    poblacion <- return (reemplazar poblacion hijos) --reemplazamos los peores individuos de la población inicial por los hijos
    poblacion <- return (seleccion poblacion) --ordenamos la población
    print poblacion --imprimimos la población final


