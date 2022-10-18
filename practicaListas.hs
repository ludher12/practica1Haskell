
--Definir la función firstToEnd tal que (firstToEnd xs) es una lista donde el primer
--elemento de xs, pasa a ser el último elemento de la nueva lista
--la funcion recibe un arrelo y retorna un arreglo
firstToEnd :: [a] -> [a]
firstToEnd  [] = []
firstToEnd (x:xs) = xs++[x]

--Definir la función minAndMax tal que (minAndMax xs) es una lista con únicamente 2
--elementos (elemento mínimo de xs y elemento máximo de xs), donde xs es una lista
--recibe una lista y retorna una lista con el numero menor y el mayor de la lista
minAndMax :: (Ord a) => [a] -> [a]
minAndMax lista = [minimum lista] ++ [maximum lista]

--Definir la función minorsFirstElement tal que (minorsFirstElement xs) es una lista con
--los elementos menores al primer elemento de xs, donde xs es una lista.(El primer
--elemento se ignora)
--recibe una lista y retorna otra lista donde se enceuntran los valores menores al primer elemento
minorsFirtsElement :: Ord a => [a] -> [a]
minorsFirtsElement [] = error "La lista no debe de estar vacia"
minorsFirtsElement (x:xs) = [y | y <- xs, y<x ] 

--Definir la función greaterOrEqualFirstElement tal que (greaterOrEqualFirstElement
--xs) es una lista con los elementos mayores o iguales al primer elemento de xs,
--donde xs es una lista.(El primer elemento se ignora)
--recibe una lista y retorna otra lista donde se enceuntran los valores mayores o iguales al primer elemento
greaterOrEqualFirstElement :: Ord a => [a] -> [a]
greaterOrEqualFirstElement [] = error "La lista no debe de estar vacia"
greaterOrEqualFirstElement (x:xs) = [y | y <- xs, y>=x]

--Definir la función minorsToSumFirstAndSecondElem tal que
--(minorsToSumFirstAndSecondElem xs) es una lista con los elementos menores a la
--suma del primer y segundo elemento de xs (sin tomar en cuenta los primeros 2
--elementos), donde xs es una lista.
--recibe una lista y retorna una lista con los numeros menores a la suma de esos numeros
minorsToSumFirstAndSecondElem :: (Integral a) => [a] -> [a]
minorsToSumFirstAndSecondElem [] = error "La lista no debe de estar vacia"
minorsToSumFirstAndSecondElem [x] = error "necesitas mas de 2 elementos"
minorsToSumFirstAndSecondElem (x:y:[]) = error "necesitas mas de 2 elementos"
minorsToSumFirstAndSecondElem (x:y:xs) = [z | z <-xs, z< (x+y)]

--Definir la función listSumDuplaToList tal que (listSumDuplaToList xs) es una lista en
--la que cada elemento es la suma de los elementos de cada dupla, donde xs es una
--lista de duplas.
--se recibe una lista de duplas y retorna una lista con las sumas de los elementos de cada supla
listSumDuplaToList :: Num a => [(a, a)] -> [a]
listSumDuplaToList [] = []
listSumDuplaToList (x:xs) = [fst x + snd x] ++ listSumDuplaToList xs

--se crea la funcion thrd para retornar el ultimo valor de la tripla
thrd :: (a, b, c) -> c
thrd (_,_,c) = c
--se crea la funcion fst' que retorna el primero elemento de la tripla
fst' :: (a, b, c) -> a
fst' (a,_,_) = a
--se creala funcion snd' que retorna el segundo elemento de la tripla
snd' :: (a, b, c) -> b
snd' (_,b,_) = b

--Definir la función listMultTripletaToList tal que (listMultTripletaToList xs) es una lista
--en la que cada elemento es la multiplicación de los elementos de cada tripleta,
--donde xs es una lista de tripletas.
--con ayuda de las funciones anteriores accedo a los valores de la tripleta y los multiplico
listMultTripletaToList :: Num a => [(a, a, a)] -> [a]
listMultTripletaToList [] = []
listMultTripletaToList (x:xs) = [fst' x * snd' x * thrd x] ++ listMultTripletaToList xs


--Definir la función changeFstToSnd tal que (changeFstToSnd xs) es una lista en
--donde los elementos de una dupla cambian de posición, donde xs es una lista de
--duplas.
--con ayuda de las funciones snd y fst cambio las posiciones de la dupla
changeFstToSnd :: [(b1, b2)] -> [(b2, b1)]
changeFstToSnd [] = []
changeFstToSnd (x:xs) = [(snd x,fst x)] ++ changeFstToSnd xs

--creo una funcion que me ayuda a retornar una lista de los primeros valores de la dupla
listFirstDupla :: [(a, b)] -> [a]
listFirstDupla [] = []
listFirstDupla (x:xs) = [fst x] ++ listFirstDupla xs
--creo una funcion que me ayuda a retornar una lista de los segundos valores de la dupla
listSndDupla :: [(a1, a2)] -> [a2]
listSndDupla [] = []
listSndDupla (x:xs) = [snd x] ++ listSndDupla xs

--Definir la función sumVectors tal que (sumVectors xs) es un vector resultante de la
--suma de los diferentes vectores de xs, donde xs es una lista de duplas.
--retorna una dupla que seria el vector resultando con la sumatoria de las 2 listas que se obtuvieron
--con las funciones anteriores
sumVectors :: (Num a1, Num b) => [(a1, b)] -> (a1, b)
sumVectors vector = (sum (listFirstDupla vector), sum (listSndDupla vector))

--creo una funcion que retorna un arreglo con los divisores y que recibe el numero y un contador
--esto para llevar un orden en la recursividad y si los numeros son divisores se agregan a la lista
divisores :: Int -> Int ->[Int]
divisores num cont 
    |cont == num = [num]
    |mod num cont == 0 = cont : divisores num (cont+1)
    |mod num cont /= 0 = divisores num (cont+1)

--Definir la función dividers tal que (dividers n) es una lista de los divisores de n, donde
--n es un número.
dividers :: Int -> [Int]
dividers num = divisores num 1

--creo la funcion eleimina para poder filtrar los numeros que son divisibles por el primer valor
elimina :: Integral a => a -> [a] -> [a]
elimina n xs = [ x | x <- xs, mod x  n /= 0 ]
--con esta funcion voy a estar llamando a la funcion elimina conforme encuentre mas numeros primos
criba :: Integral a => [a] -> [a]
criba []     = []
criba (n:ns) = n : criba (elimina n ns)
--Definir la función primeNumbers tal que (primeNumbers n) es una lista con los
--números primos existentes de 1 a n, donde n es un número.
--Recuerda: un número primo tiene únicamente 2 divisores 1 y el mismo número.
--esta funcion se realiza llamando a las funciones auxiliares creadas anterioemente
primeNumbers :: Integral a => a -> [a]
primeNumbers numero = criba [2..numero] 

--Definir la función infinitePrimeNumbers tal que (infinitePrimeNumbers) es una lista
--infinita de los números primos.
--hago lo mismo que en el anterior pero sin un fin
primos :: [Integer]
primos = criba [2..]
         where criba (p:ps) = p : criba [n | n<-ps, mod n p /= 0]
