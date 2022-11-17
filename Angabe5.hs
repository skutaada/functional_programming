module Angabe5 where

import Data.List

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Ord, Show)

data Skonto  = Kein_Skonto
               | DreiProzent  
               | FuenfProzent
               | ZehnProzent

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq, Ord)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq, Ord)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq, Ord)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq, Ord)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr
                        } deriving (Eq)

newtype Lieferausblick = LA [(Lieferfenster,Nat0)]

data Datensatz
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment


newtype Sortiment = Sort [(Typ,Datensatz)]

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq, Ord, Show)

newtype Anbieter = A [(Haendler,Sortiment)]

type Suchanfrage = Typ  

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


-- Aufgabe A.1

instance Wgf Lieferausblick where
 ist_wgf (LA []) = True
 ist_wgf (LA x) = and (checkArray x)
   where 
      checkArray [] = []
      checkArray ((f, s):lf) = (and $ map (==s) $ map snd $ filter (\(a, b) -> (a == f)) lf) : checkArray lf 
 wgf_fehler _ = error "Ausblickfehler"

sortimentHelper :: [(Typ, Datensatz)] -> Bool
sortimentHelper [] = True
sortimentHelper ((_, s):[]) = ist_wgf $ lieferbare_stueckzahl_im_Zeitfenster s
sortimentHelper ((f1, s1):(f2, s2):lf)
   | f1 /= f2 && (ist_wgf $ lieferbare_stueckzahl_im_Zeitfenster s1) && (ist_wgf $ lieferbare_stueckzahl_im_Zeitfenster s2) = sortimentHelper ((f2,s2):lf)
   | otherwise = False

instance Wgf Sortiment where
 ist_wgf (Sort x) = sortimentHelper $ sortOn fst x
 wgf_fehler _ = error "Sortimentfehler"

anbieterHelper :: [(Haendler, Sortiment)] -> Bool
anbieterHelper [] = True
anbieterHelper ((_, s):[]) = ist_wgf s
anbieterHelper ((f1, s1):(f2, s2):lf)
   | f1 /= f2 && ist_wgf s1 && ist_wgf s2 = anbieterHelper ((f2, s2):lf)
   | otherwise = False

instance Wgf Anbieter where
  ist_wgf (A x) = anbieterHelper $ sortOn fst x
  wgf_fehler _ = error "Anbieterfehler"

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}

checkSortiment :: Suchanfrage -> Sortiment -> Bool
checkSortiment _ (Sort []) = False
checkSortiment t (Sort ((ts, d):lf))
   | t == ts && (sofort_lieferbare_stueckzahl d) > 0 = True
   | otherwise = checkSortiment t (Sort lf)

-- Aufgabe A.5

type Haendlerliste = [Haendler]

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig t a@(A x)
   | ist_wgf a = sofort_lieferfaehig' t (A (reverse $ sortOn fst x))
   | otherwise = error "Anbieterfehler"

sofort_lieferfaehig' :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig' _ (A []) = []
sofort_lieferfaehig' t (A ((h, s):lf))
   | checkSortiment t s = h : sofort_lieferfaehig' t (A lf)
   | otherwise = sofort_lieferfaehig' t (A lf)

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl t a
   | ist_wgf a = sofort_erhaeltliche_Stueckzahl' t a (0, 0)
   | otherwise = error "Anbieterargumentfehler"

sofort_erhaeltliche_Stueckzahl' :: Suchanfrage -> Anbieter -> (Stueckzahl, Gesamtpreis) -> (Stueckzahl, Gesamtpreis)
sofort_erhaeltliche_Stueckzahl' _ (A []) (sz ,gp) = (sz, gp) 
sofort_erhaeltliche_Stueckzahl' t (A ((_, s):lf)) (sz, gp) = sofort_erhaeltliche_Stueckzahl' t (A lf) (sz + x, gp + y)
   where (x, y) = checkSortimentAndCount t s

checkSortimentAndCount :: Suchanfrage -> Sortiment -> (Stueckzahl, Gesamtpreis)
checkSortimentAndCount t (Sort []) = (0, 0)
checkSortimentAndCount t (Sort ((ts, d):lf))
   | t == ts = (sofort_lieferbare_stueckzahl d, (sofort_lieferbare_stueckzahl d) * (preis_in_euro d))
   | otherwise = checkSortimentAndCount t (Sort lf)
{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten t lf a@(A x)
   | ist_wgf a = guenstigste_Lieferanten' t lf (A (reverse $ sortOn fst x))
   | otherwise = error "Anbieterargumentfehler"

guenstigste_Lieferanten' :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten' t lf an
   | prices == [] = Nothing
   | otherwise = Just (map fst (filter (\(a,b) -> b == minPrice) prices))
   where prices = checkAll t lf an
         minPrice = minimum $ map snd prices

checkAll :: Suchanfrage -> Lieferfenster -> Anbieter -> [(Haendler, Nat1)]
checkAll _ _ (A []) = []
checkAll t lf (A ((h, s):lt))
   | x /= -1 = (h, x) : checkAll t lf (A lt)
   | otherwise = checkAll t lf (A lt)
   where x = checkOne t lf s

checkOne :: Suchanfrage -> Lieferfenster -> Sortiment -> Nat1
checkOne t lf (Sort []) = -1
checkOne t lf (Sort ((ts, d):lt))
   | t == ts && (checkLiefer (lieferbare_stueckzahl_im_Zeitfenster d) lf) > 0 = preis_in_euro d
   | otherwise = checkOne t lf (Sort lt)
   where checkLiefer (LA l) lf 
            | x == [] = 0
            | otherwise = head x
            where x = map snd $ filter (\(a,b) -> (a == lf)) l


{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster t lf sz a@(A x)
   | ist_wgf a = guenstigste_Lieferanten_im_Lieferfenster' t lf sz (A (reverse $ sortOn fst x))
   | otherwise = error "Anbieterargumentfehler"

guenstigste_Lieferanten_im_Lieferfenster' :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster' t lf sz an = filter (\(a,b) -> b == minPrice) prices
   where prices = checkAllRabbat t lf sz an
         minPrice = minimum $ map snd prices

checkAllRabbat :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler, RabattierterPreis)]
checkAllRabbat _ _ _ (A []) = []
checkAllRabbat t lf sz (A ((h, s):lt))
   | euro x /= -1 = (h, x) : checkAllRabbat t lf sz (A lt)
   | otherwise = checkAllRabbat t lf sz (A lt)
   where x = checkOneRabbat t lf sz s

checkOneRabbat :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Sortiment -> RabattierterPreis
checkOneRabbat _ _ _ (Sort []) = EUR {euro=(-1)}
checkOneRabbat t lf sz (Sort ((ts, d):lt))
   | t == ts && (checkLiefer (lieferbare_stueckzahl_im_Zeitfenster d) lf) >= sz = calculateRabbat (skonto d) (preis_in_euro d) sz
   | otherwise = checkOneRabbat t lf sz (Sort lt)
   where checkLiefer (LA l) lf
            | x == [] = 0
            | otherwise = head x
            where x = map snd $ filter (\(a,b) -> (a == lf)) l

calculateRabbat :: Skonto -> Nat1 -> Nat0 -> RabattierterPreis
calculateRabbat Kein_Skonto p s = EUR {euro=((ceiling (fromIntegral(p*s)/10)) * 10)}
calculateRabbat DreiProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.97)/10))*10)}
calculateRabbat FuenfProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.95)/10))*10)}
calculateRabbat ZehnProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.9)/10))*10)}



{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

