
--Ejercicio 1 average3Numbers
--obtener el promedio de 3 numeros datos por el usuario, recibe 3 numeros fraccionales 
--y retorna otro numero fraccional que contiene el resultado
average3Numbers :: (Fractional a )=> a -> a -> a -> a
average3Numbers x y z = (x+y+z)/3

--Ejercicio 2 isLastDigit3
--identificar que el ultimo digito de un numero termina en 3
-- recibe un numero de tipo integral y retorna un valor booleano
isLastDigit3 :: (Integral a) => a -> Bool
isLastDigit3 num 
    | mod (num-3) 10 ==0 = True
    |otherwise = False

--Ejercicio 3 has3Digits 
--determinar si en numero dado es de 3 digitos
--recibe un numero integral y retorna un booleano
has3Digits :: (Integral a) => a -> Bool
has3Digits num 
    |num>99 && num <1000 = True
    |otherwise = False


--Ejercicio 4 isNegative
--identificar si el numero ingresado es negativo o no
--recibe un numero de tipo integral y retorna un booleano
isNegative :: (Integral a) => a -> Bool
isNegative num 
    | num <0 = True
    |otherwise = False


--Ejercicio 5 sum2Digits
--retornar la suma de los 2 digitos de un numero, solo deben de ser 2 digitos por cifra
-- ingresamos el numero de 2 cifras, en dado caso que sean mas o menos de 2 cifras retorna un error
-- si el numero es de 2 digitos retorna la suma de esos digitos, auxiliado con un show y un read
--para asi obtener la primer posicion del digito y la ultima posicion y sumarlas
sum2Digits :: (Ord a, Num a, Num p, Read p, Show a) => a -> p
sum2Digits num
    | num>99 || num<10 = error "el numero debe de ser de 2 digitos"
    |otherwise = read (init (show num)) + read (tail(show num) )+0


--Ejercicio 6 even2Digits
--determinar si ambos digitos de un numero de 2 digitos son pares
--ingresamos el numero de 2 cifras, en dado caso que sean mas o menos de 2 cifras retorna un error
--si ambos digitos son pares entonces retornara true, de lo contrario false, auxiliado con un show y un read
--para asi obtener la primer posicion del digito y la ultima posicion y sumarlas
even2Digits :: (Show a, Num a, Ord a) => a -> Bool
even2Digits num
    | num>99 || num<10 = error "el numero debe de ser de 2 digitos"
    |otherwise = even(read (init (show num)) +0) && even(read (tail(show num) )+0)


-- ejercicio 7 isPrimeNumber
--identificar si el numero ingresado es de los primeros 8 numeros primos
-- si es primo retorna true de lo contrario false
--ingresamos un numero y retorna un booleano
isPrimeNumber :: (Integral a) => a -> Bool
isPrimeNumber num 
    |num ==2 = True
    |num ==3 = True
    |num ==5 = True
    |num ==7 = True
    |num ==11 = True
    |num ==13 = True
    |num ==17 = True
    |num ==19 = True
    |otherwise = False
    
--Ejercicio 8 isEvenAndPrimeNumber
--identifica si el numero es primo y par
--el unico numero que cumple esa condicion es 2, asi que si el numero es 2 retorna true, si no false
--recibe un numero y retorna un booleano
isEvenAndPrimeNumber :: (Integral a) => a -> Bool
isEvenAndPrimeNumber num
    |num ==2 = True
    |otherwise = False


--Ejercicio 9 isMultiple
--determina si el primer valor  ingresado es multiplo del segundo valor
--si es multiplo retorna true, sino false
--recibe 2 numeros y retorna un booleano 
isMultiple :: Integral a => a -> a -> Bool
isMultiple x y = mod x y ==0

--Ejercicio 10 isEquals2Digits 
--Determina si los digitos de un numero de 2 digitos son iguales
--Si son iguales retorna true, sino false, auxiliado con un show y un read
--para asi obtener la primer posicion del digito y la ultima posicion y podercompararlas
--recibe un numero de 2 cifras, sino es de 2 cifras retorna un error y si si es retorna un booleano
isEquals2Digits :: (Ord a, Num a, Show a) => a -> Bool
isEquals2Digits num 
    | num>99 || num<10 = error "el numero debe de ser de 2 digitos"
    |otherwise = read (init (show num) )+0 == read (tail(show num) )+0

--Ejercicio 11 higher
--determinar cual es el numero mayor entre 3 numeros
--recibe 3 numeros y retorna al mayor de esos 3 numeros 
higher :: (Integral a )=> a -> a -> a -> a
higher x y z = max x (max y z)

--Ejerciocio 12 isEvenSum2Number
--determina si la suma de 2 numeros es par
--si la suma es par retorna true, de lo contrario false
--recibe 2 numeros y retorna un booleano 
isEvenSum2Number :: Integral a => a -> a -> Bool
isEvenSum2Number x y = even (x + y)

-- Ejercicio 13 sum2Digit2Number
--detemermina la suma de los 2 digitos de 2 numeros, ambos numeros son de 2 digitos
--regresa la suma de los numeros, si algun numero no es de 2 digitos genera un error, auxiliado con un show y un read
--para asi obtener la primer posicion del digito y la ultima posicion y sumarlas
sum2Digit2Number :: (Ord a1, Ord a2, Num a1, Num a2, Num p, Read p, Show a1,
 Show a2) =>a1 -> a2 -> p
sum2Digit2Number x y 
    | x<10 || x>99 || y<10 || y>99 = error "Ambos numeros deben de ser de 2 digitos"
    |otherwise = read (init (show x)) + read (tail(show x) )+0 + read (init (show y)) + read (tail(show y) )

--Ejercicio 14 sum3Digits
--determina la suma de lso 3 digitos de un numero
-- si el numero no es de 3 digitos genera un error y si si la suma, auxiliado con un show y un read
--para asi obtener las 3 posiciones de los digitos y sumarlas
sum3Digits :: (Ord a, Num a, Num p, Read p, Show a) => a -> p
sum3Digits num
    |num >1000 || num<100 = error"El numero debe de ser de 3 digitos"
    |otherwise = read (init(init (show num))) + read (tail(tail(show num)))+ read (init(tail(show num)))+0

--Ejercicio 15 equal3Digits
equal3Digits :: (Ord a, Show a, Num a) => a -> Bool
equal3Digits num
    |num >1000 || num<100 = error"El numero debe de ser de 3 digitos"
    | (read (init(init (show num)))+0) == (read (tail(tail(show num)))+0)= True
    | (read (tail(tail(show num)))+0) == (read (init(tail(show num)))+0) = True
    | (read (init(init (show num)))+0) == (read (init(tail(show num)))+0) = True
    | otherwise = False

--Ejercicio 16 positionHigher3Digits
positionHigher3Digits :: (Show a, Ord a, Num a) => a -> [Char]
positionHigher3Digits num
    |num >1000 || num<100 = error"El numero debe de ser de 3 digitos"
    |(read (init(init (show num)))+0) == (read (tail(tail(show num)))+0) && (read (init(init (show num)))+0) == (read (init(tail(show num)))+0) = "los 3 digitos son iguales"
    |(read (init(init (show num)))+0) > (read (tail(tail(show num)))+0) && (read (init(init (show num)))+0) > (read (init(tail(show num)))+0) = "El mayor se encuentra en la posicion 1"
    |(read (tail(tail(show num)))+0) > (read (init(init (show num)))+0) && (read (tail(tail(show num)))+0)  > (read (init(tail(show num)))+0) = "El mayor se encuentra en la posicion 3"
    |otherwise = "El mayor se encuentra en la posicion 2"

--Ejercicio 17 palindrome
palindrome :: Eq a => [a] -> Bool
palindrome arreglo = arreglo == reverse arreglo

--Ejercicio 18 safeDivision
safeDivision :: (Eq p, Fractional p) => p -> p -> p
safeDivision x 0 = error "no se puede dividir entre o"
safeDivision x y = x/y

--Ejercicio 19 xor
xor :: Bool -> Bool -> Bool
xor x y = not(x && y)

--Ejercicio 20 distance
distance :: Floating a => (a, a) -> (a, a) -> a
distance x y = sqrt( (fst y - fst x)^2 + (snd y - snd x)^2)