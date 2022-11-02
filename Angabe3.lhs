> module Angabe3 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen muessen durch mindestens eine Leerzeile getrennt sein!


Als erstes fuehren wir den Typalias Nat1 ein; seine Werte verwenden wir fuer
die Darstellung des Typs von Matrizen:

> type Nat1 = Int
> type Typ  = (Nat1,Nat1)

Matrizen modellieren wir als Listen von (Matrix-) Zeilen ueber entsprechenden
selbstdefierten Listentypen:

Zur Namenswahl: LE fuer `Letztes Element', E fuer `Element'

> data Zeile = LE Int                       
>              | E Int Zeile deriving Show

Zur Namenswahl: LZ fuer `Letzte Zeile, Z fuer `Zeile'

> data Matrix = LZ Zeile                       
>               | Z Zeile Matrix deriving Show  

Um mit Argumenten umzugehen, die keine Matrix darstellen oder im Typ nicht
zueinander passen, fuehren wir den Wert fehler als fehleranzeigenden Wert
ein (aufgrund unserer Festlegung von fehler bedeutet das, dass die Rueckgabe
dieses Werts gleichbedeutend mit dem Aufruf der Funktion error mit dem 
Argument "Argument(e) typfehlerhaft" ist und die Programmausfuehrung mit 
Ausgabe der Zeichenreihe "Argument(e) typfehlerhaft" endet).

> fehler = error "Argument(e) typfehlerhaft"

Abschliessend fuehren wir den algebraischen Datentyp Matrixtyp ein:

> data Matrixtyp = Matrix_vom_Typ Typ 
>                   | KeineMatrix deriving (Eq,Show)


Aufgabe A.1

> countCols :: Zeile -> Int
> countCols (LE _) = 1
> countCols (E _ z) = 1 + countCols z

> countRows :: Matrix -> (Int, Int)
> countRows (LZ z) = (1, countCols z)
> countRows (Z z m)
>    | countCols z == s = (r+1, s)
>    | otherwise = (0, 0)
>    where (r, s) = countRows m

> matrixtyp :: Matrix -> Matrixtyp
> matrixtyp m
>   | size == (0, 0) = KeineMatrix
>   | otherwise = Matrix_vom_Typ size
>   where size = countRows m

Knapp, aber gut nachvollziebar geht matrixtyp folgendermassen vor: 
    This function calculates the size of matrix m by using two helper functions for counting collumns and rows while making sure that the number of collumns does not change throughout the rows.
    If the the number of collumns mismatch then returns KeineMatrix


Aufgabe A.2

> cmpRows :: Zeile -> Zeile -> Bool
> cmpRows (LE i1) (LE i2) = i1 == i2
> cmpRows (E i1 z1) (E i2 z2)
>   | i1 == i2 = cmpRows z1 z2
>   | otherwise = False

> cmpCols :: Matrix -> Matrix -> Bool
> cmpCols (LZ z1) (LZ z2) = cmpRows z1 z2
> cmpCols (Z z1 m1) (Z z2 m2)
>   | cmpRows z1 z2 = cmpCols m1 m2
>   | otherwise = False

> instance Eq Matrix where
>  m1 == m2
>   | matrixtyp m1 == KeineMatrix = fehler
>   | matrixtyp m2 == KeineMatrix = fehler
>   | matrixtyp m1 /= matrixtyp m2 = fehler
>   | otherwise = cmpCols m1 m2
>  m1 /= m2
>   | matrixtyp m1 == KeineMatrix = fehler
>   | matrixtyp m2 == KeineMatrix = fehler
>   | matrixtyp m1 /= matrixtyp m2 = fehler
>   | cmpCols m1 m2 = False
>   | otherwise = True

Knapp, aber gut nachvollziehbar geht die Instanzdeklaration fuer Eq folgendermassen vor:
    In case of equality compares matrices value by value and returns True if they match.
    In case of inequality just inverts the result of equality
 

Aufgabe A.3

> applyFunctionToRow :: (Int -> Int -> Int) -> Zeile -> Zeile -> Zeile
> applyFunctionToRow f (LE i1) (LE i2) = LE (f i1 i2)
> applyFunctionToRow f (E i1 z1) (E i2 z2) = E (f i1 i2) (applyFunctionToRow f z1 z2)

> applyFunctionToCol :: (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
> applyFunctionToCol f (LZ z1) (LZ z2) = LZ (applyFunctionToRow f z1 z2)
> applyFunctionToCol f (Z z1 m1) (Z z2 m2) = Z (applyFunctionToRow f z1 z2) (applyFunctionToCol f m1 m2)

> applyAbsToRow :: Zeile -> Zeile
> applyAbsToRow (LE i1) = LE (abs i1)
> applyAbsToRow (E i z) = E (abs i) (applyAbsToRow z)

> applyAbsToCol :: Matrix -> Matrix
> applyAbsToCol (LZ z) = LZ (applyAbsToRow z)
> applyAbsToCol (Z z m) = Z (applyAbsToRow z) (applyAbsToCol m)

> instance Num Matrix where
>  m1 + m2
>   | matrixtyp m1 == KeineMatrix = fehler
>   | matrixtyp m2 == KeineMatrix = fehler
>   | matrixtyp m1 /= matrixtyp m2 = fehler
>   | otherwise = applyFunctionToCol (+) m1 m2
>  m1 - m2
>   | matrixtyp m1 == KeineMatrix = fehler
>   | matrixtyp m2 == KeineMatrix = fehler
>   | matrixtyp m1 /= matrixtyp m2 = fehler
>   | otherwise = applyFunctionToCol (-) m1 m2
>  abs m
>   | matrixtyp m == KeineMatrix = fehler
>   | otherwise = applyAbsToCol m
>  fromInteger z = LZ (LE (fromInteger z))
>  m1 * m2 = error "(*) bleibt unimplementiert!"
>  negate m = error "negate bleibt unimplementiert!"
>  signum m = error "signum bleibt unimplementiert!"

Knapp, aber gut nachvollziebar geht die Instanzdeklaration fuer Num folgendermassen vor:
    Define sum of matrices value by value, substraction value by value and absolute matrix by applying abs function to each element.






