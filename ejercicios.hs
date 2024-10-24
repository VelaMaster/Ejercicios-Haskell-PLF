-- Funciones Basicas
-- Ejercicio 1
promedio3 :: Double -> Double -> Double -> Double
promedio3 x y z = (x + y + z)/3

-- Ejercicio 2
sumaMonedas :: Int -> Int -> Int -> Int -> Int -> Int
sumaMonedas mon1 mon2 mon3 mon4 mon5 = (mon1 * 1)+(mon2*2)+(mon3*5)+(mon4*10)+(mon5*20)

--Ejercicio 3
volumenEsfera :: Double -> Double
volumenEsfera r = ((4/3) * (3.1416 * r^3))

--Ejercicio 4
areaCorona :: Double -> Double -> Double
areaCorona rR r = 3.1416*(r^2 - rR^2)

--Ejercicio 5
ultimaCifra :: Int -> Int
ultimaCifra x = rem x 10

--Ejercicio 6
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

--Ejercicio 7
rota1 :: [a] -> [a]
rota1 xs = tail xs ++ [head xs]

--Ejercicio 8
rotarTal :: Int -> [a] -> [a]
rotarTal n (xs) = drop n (xs) ++ take n (xs)

--Ejercicio 9
rangoxs ::(Ord a) => [a] -> [a]
rangoxs (xs) = [minimum xs, maximum xs]

--Ejercicio 10
palindromo :: (Eq a) => [a] -> Bool
palindromo (xs) = (xs) == reverse (xs)

--Ejercicio 11
interiorxs :: [a] -> [a]
interiorxs (xs) = init (tail xs)

--Ejercicio 12
--Ejercicio 13
segmentoTal :: Int -> Int -> [a] -> [a]
segmentoTal m n (xs) = drop (m-1) (take n xs)

--Ejercicio 14
extremos :: Int -> [a] -> [a]
extremos num (xs) = take num xs ++ take num (drop (length xs - num)xs)

--Ejercicio 15 
mediano :: Int -> Int -> Int -> Int
mediano x y z = (x + y + z) - (maximum [x,y,z] + minimum[x,y,z])

--Ejercicio 16
tresIguales :: Double -> Double -> Double -> Bool
tresIguales x y z = ((x + y + z)/3) == x 

--Ejercicio 17
tresDiferentes :: Double -> Double -> Double -> Bool
tresDiferentes x y z = (x /= y) && (x /= z) && (y /= z)

--Ejercicio 18
cuatroIguales :: Double -> Double -> Double -> Double -> Bool
cuatroIguales x y z w = tresIguales x y z && x == w 


-- Guardas y patrones
--Ejercicio 1
divisionSegura :: Double -> Double -> Double
divisionSegura x y
 | y == 0 = 9999
 | otherwise = x/y

--Ejercicio 2
disyuncion :: Bool -> Bool -> Bool
disyuncion v1 v2
 | v1 == v2 = False
 | v1 /= v2 = True

--Ejercicio 3
mayorRectangulo :: (Integer, Integer) -> (Integer, Integer) -> (Integer,Integer)
mayorRectangulo (x1,y1) (x2,y2)
 | (x1 * y1) > (x2 * y2) = (x1,y1)
 | otherwise = (x2,y2)

--Ejercicio 4
intercambia :: (a,b) -> (b,a)
intercambia (a,b) = (b,a)

--Ejercicio 5
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--Ejercicio 6
ciclo :: [a] -> [a]
ciclo [] = []
ciclo [x] = [x]
ciclo (xs) = last xs : init xs

--Ejercicio 7
numeroMayor :: (Num a, Ord a) => a -> a -> a 
numeroMayor x y
 | 10*x+y > 10*y+x = 10*x+y
 | otherwise = 10*y+x

--Ejercicio 8
numerodeRaices :: (Floating t, Ord t) => t -> t -> t -> Int
numerodeRaices a b c
 | d > 0 = 2
 | d == 0 = 1
 | otherwise = 0
 where
 d = b^2 - 4 * a * c

--Ejercicio 9 
raices :: Double -> Double -> Double -> [Double]
raices a b c
  | d > 0  = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
  | d == 0 = [-b / (2 * a)]
  | otherwise          = []
  where
    d = b^2 - 4 * a * c

--Ejercicio 10
area :: Double -> Double -> Double -> Double
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
 where
 s = (a+b+c)/2

--Ejercicio 11
interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1,b1] [a2,b2]
 | c <= d = [c,d]
 | otherwise = []
 where
 c = max a1 a2
 d = min b1 b2

--Ejercicio 12
linea n = [inicio..fin]
 where
  inicio = (n*(n-1)) `div` 2 + 1
  fin = inicio + n -1


--Recursividad 
--Ejercicio 1
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1 
potencia num pot = num * potencia num (pot - 1)

--Ejercicio 2
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

--Ejercicio 3
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece num (x:xs) = x == num || pertenece num xs

--Ejercicio 4
tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar toma (x:xs) = [x] ++ tomar (toma - 1) xs

--Ejercicio 5
digitosC :: Integer -> [Integer]
digitosC 0 = []
digitosC num = digitosC (num `div` 10) ++ [num `mod` 10] 

--Ejercicio 6 
sumaDigitosR :: Integer -> Integer
sumaDigitosR 0 = 0
sumaDigitosR num = (num `mod` 10) + sumaDigitosR (num `div` 10)

--Ejercicio 2.1 
ordenaRapido :: Ord a => [a] -> [a]
ordenaRapido [] = []
ordenaRapido (x:xs) = ordenaRapido izq ++ [x] ++ ordenaRapido der
 where 
 izq = [y| y <- xs, y <= x]
 der = [y| y <- xs,y > x]

--Nuevos tipos de datos
data Estudiante = Estudiante String String Double Int deriving Show

listaOrdenadaA :: [Estudiante] -> [Estudiante]
listaOrdenadaA [] = []
listaOrdenadaA (Estudiante no ap ed ma:xs) = listaOrdenadaA izq ++ [Estudiante no ap ed ma] ++ listaOrdenadaA der
 where
 izq = [Estudiante n a e m | Estudiante n a e m <- xs , e <= ed]
 der = [Estudiante n a e m | Estudiante n a e m <- xs, e > ed]

estudianteMayor :: [Estudiante] -> Estudiante
estudianteMayor [x] = x
estudianteMayor (Estudiante n a e m : xs) = if e > e' then Estudiante n a e m else estudianteMayor xs
 where
 Estudiante _ _ e' _ = estudianteMayor xs 

estudianteMenor :: [Estudiante] -> Estudiante
estudianteMenor [x] = x
estudianteMenor (Estudiante n a e m : xs) = if e < e' then Estudiante n a e m else estudianteMenor xs
 where
 Estudiante _ _ e' _ = estudianteMenor xs

estudianteMayMen :: [Estudiante] -> [Estudiante]
estudianteMayMen (xs) = [estudianteMenor xs, estudianteMayor xs]

edadSuma :: [Estudiante] -> (Double,Double)
edadSuma [] = (0,0)
edadSuma (Estudiante n a e m:xs) = (e + totalSuma, contador + 1)
 where
 (totalSuma, contador) = edadSuma xs

edadPromedio :: (Double, Double) -> Double
edadPromedio (suma,division) = suma/division

edad :: [Estudiante] -> Double
edad (xs) = edadPromedio (edadSuma xs)


data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a) deriving (Eq, Show)
generarNodo :: a -> Arbol a
generarNodo x  = Nodo x Hoja Hoja

insertar x Hoja = generarNodo x
insertar x (Nodo a izq der)
 | x < a = Nodo a (insertar x izq) der
 | x > a = Nodo a izq (insertar x der)
 | otherwise = Nodo a izq der

insertarLista [] arbol = arbol
insertarLista (x:xs) arbol = insertarLista xs arbolNuevo
 where
  arbolNuevo = insertar x arbol

buscarArbol x Hoja = False
buscarArbol x (Nodo a izq der)
 | x == a = True
 | x < a = buscarArbol x izq
 | x > a = buscarArbol x der

inOrden Hoja = []
inOrden (Nodo a izq der) = inOrden izq ++ [a] ++ inOrden der

preOrden Hoja = []
preOrden (Nodo a izq der) = [a] ++ preOrden izq ++ preOrden der

postOrden Hoja = []
postOrden (Nodo a izq der) = postOrden izq ++ postOrden der ++ [a]