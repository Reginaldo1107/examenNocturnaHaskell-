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


--QUIERO TODAS FALSE 
pruebaEquipoYArqueroIguales =  [("BOCA","BOCA") , ("RA","RE") , ("RI","RO")] 
pruebaTuplasIguales =[("BOCA","RIVER") , ("BOCA","RIVER") , ("RI","RO")]
pruebaAlrevesEquipoYArquero =  [("BOCA","RA") , ("RA","BOCA") , ("RI","RO")]

pruebaMismoEquipoDistintoArquero =  [("BOCA","RA") , ("BOCA","RE") , ("RI","RO")]
pruebaMismoArqueroDistintoEquipo = [("SAL","RE") , ("SEL","RE") , ("RI","RO")]
pruebaIgualArqueroConEquipo = [("SAL","SEL") , ("SEL","RE") , ("RI","RO")]

