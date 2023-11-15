{--

[ String , String ] == [" NOMBRE DEL EQUIPO " , " NOMBRE DEL ARQUERO "]
EJEMPLO :
[(" SACACHISPA "," NEYDER ARAGON ")] [3]
[("   FENIX   "," NAHUEL GALARDI ")] [5]

--> NEYDER ARAGON RECIBIO 3 GOLES 
--> NAHUEL GALARDI RECIBIO 5 GOLES
--}

--1)ATAJARON SUPLENTES [1 PUNTO] 

sumatoria :: [Int] -> Int 
sumatoria [] = 0 
sumatoria [x] = x 
sumatoria (x:xs) = x  + sumatoria xs  

atajaronSuplentes :: [(String,String)] -> [Int] -> Int -> Int 
atajaronSuplentes _ golesTitulares total = total -  sumatoria golesTitulares


--2 ) EQUIPOS VALIDOS [3 PUNTOS]
{--
arquerosPorEquipo no contiene nombres de clubes repetidos , ni arqueros repetidos , ni jugadores con nombre de club 
--}
hayRepetidos :: String -> [(String, String)] -> Bool
hayRepetidos c [] = False 
hayRepetidos c ((x,y):xs)   | c == x = True
                            | c == y = True
                            | otherwise = hayRepetidos c xs  


equiposValidos :: [(String, String)] -> Bool
euqiposValidos [] = True
equiposValidos ((x,y):xs)   | x == y = False     
                            |otherwise =  not (hayRepetidos x xs) && not (hayRepetidos y xs) && equiposValidos xs


--3) Porcentaje de Goles [3 puntos]
-- Busco al arquero no al equipo 
division :: Int ->Int ->Float
division a b = (fromIntegral a) / (fromIntegral b)

arqueroElegido :: String -> [(String,String)] -> [Int] -> Int
arqueroElegido s [(x,y)] [t] = t
arqueroElegido s ((x,y):xs) (n:ns)  | s == y = n 
                                    |otherwise = arqueroElegido s xs ns

porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeGoles arquero arquerosPorEquipo goles = division ( (arqueroElegido arquero arquerosPorEquipo goles)*100 )  (sumatoria goles)


--pruebaPorcentaje = "NALDO"  [("RA","NA"),("RE","NA"),("RI","NALDO"),("RO","TA"),("RU","SA")] [1,2,3,4,5]

--VALLA MENOS VENCIDA [3 PUNTOS]

--problema vallaMenosVencida (arquerosPorEquipo , goles) -> String 
-- asegura : es alguno de los arqueros de arquerosPorEquipo que menos goles recibio de acuerdo a goles 

encontrarElMenor :: [Int] -> Int 
encontrarElMenor [x] = x 
encontrarElMenor (x:y:xs)   | x <= y = encontrarElMenor (x:xs)
                            |otherwise = encontrarElMenor (y:xs)



vallaMenosVencida :: [(String,String)] -> [Int] -> String 
vallaMenosVencida ((x,y):xs) (n:ns) | n == encontrarElMenor (n:ns) = y
                                    |otherwise = vallaMenosVencida xs ns 



arquero = [("RA","LA"),("RE","LE"),("RI","LI"),("RO","LO"),("RU","LU")]
pruebaCreciente :: [Int]
pruebaCreciente = [1,2,3,4,5]

pruebaDecreciente :: [Int]
pruebaDecreciente = [5,4,3,2,1]

pruebaCreceyDecre :: [Int]
pruebaCreceyDecre = [1,2,6,3,2]

pruebaCreceDecreCre :: [Int]
pruebaCreceDecreCre=[1,2,6,3,8]

pruebaDecreCre  ::[Int]
pruebaDecreCre = [9,8,7,12,13,14]

pruebaDecreCreDecre ::[Int]
pruebaDecreCreDecre = [9,8,14,6,4]












--QUIERO TODAS FALSE 
pruebaEquipoYArqueroIguales =  [("BOCA","BOCA") , ("RA","RE") , ("RI","RO")] 
pruebaTuplasIguales =[("BOCA","RIVER") , ("BOCA","RIVER") , ("RI","RO")]
pruebaAlrevesEquipoYArquero =  [("BOCA","RA") , ("RA","BOCA") , ("RI","RO")]

pruebaMismoEquipoDistintoArquero =  [("BOCA","RA") , ("BOCA","RE") , ("RI","RO")]
pruebaMismoArqueroDistintoEquipo = [("SAL","RE") , ("SEL","RE") , ("RI","RO")]
pruebaIgualArqueroConEquipo = [("SAL","SEL") , ("SEL","RE") , ("RI","RO")]

