module Angabe1 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
-}

-- Fuer A.1

type Nat0         = Int
type Zeichen      = Char
type Zeichenreihe = [Zeichen]
type Haeufigkeit  = Nat0
type Histogramm   = [(Zeichen,Haeufigkeit)]

-- Fuer A.2

type Gewicht        = Nat0
type Gewichtsverzeichnis = [(Zeichen,Gewicht)]
fehlerwert = -1


-- Aufgabe A.1

haeufigkeit :: Zeichenreihe -> Histogramm

{- Knapp, aber gut nachvollziehbar geht haufigkeit folgendermassen vor:
   ...
-}
haeufigkeit [] = []
haeufigkeit n = [(x, y) | x <- rmDup n, let y = (length.filter (==x)) n, y>0]
   where rmDup [] = []
         rmDup (x:xs) = x : filter (/=x) (rmDup xs)

-- Aufgabe A.2

gewicht :: Zeichenreihe -> Gewichtsverzeichnis -> Gewicht


{- Knapp, aber gut nachvollziehbar geht gewicht folgendermassen vor:
   ...
-}
gewicht x [] = 0
gewicht [] y = 0
gewicht (x:xs) y
    | (length histo) /= (sum $ map snd $ histo) = fehlerwert
    | otherwise = findWeight x y + gewicht xs y
    where histo = haeufigkeit $ map fst y
          findWeight c [] = 0
          findWeight c (g:gs)
            | c == fst g = snd g
            | otherwise = findWeight c gs


-- Aufgabe A.3

korrigiere :: Gewichtsverzeichnis -> Gewichtsverzeichnis

{- Knapp, aber gut nachvollziehbar geht korrigiere folgendermassen vor:
   ...
-}
korrigiere [] = []
korrigiere (x:xs) = x : filter (\(a,b) -> (a /= fst x)) (korrigiere xs)


-- Aufgabe A.4

korrigiere' :: Gewichtsverzeichnis -> Gewichtsverzeichnis

{- Knapp, aber gut nachvollziehbar geht korrigiere' folgendermassen vor:
   ...
-}
korrigiere' [] = []
korrigiere' (x:xs) = (fst x, snd x + (sum $ map snd $ filter (\(a,b) -> (a == fst x)) (xs))) : (korrigiere' $ deleteKeys (fst x) xs)
    where deleteKeys k [] = []
          deleteKeys k (c:cs)
            | k == fst c = deleteKeys k cs
            | otherwise = c : deleteKeys k cs