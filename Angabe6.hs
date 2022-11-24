module Angabe6 where

import Data.List

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1

newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Ord, Show)

data Skonto  = Kein_Skonto 
               | DreiProzent  
               | FuenfProzent 
               | ZehnProzent

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq, Ord, Bounded, Enum)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq, Ord, Bounded, Enum)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq, Ord, Bounded, Enum)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq, Ord)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show, Bounded, Enum)
data Jahr          = J2023 | J2024 | J2025 deriving (Eq, Ord, Show, Bounded, Enum)
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq)

newtype Lieferausblick = LA (Lieferfenster -> Nat0)

data Datensatz 
   = DS { preis_in_euro :: Nat1,
          sofort_lieferbare_stueckzahl :: Nat0,
          lieferbare_stueckzahl_im_Zeitfenster :: Lieferausblick,
          skonto :: Skonto
        }
     | Nicht_im_Sortiment

instance Eq Datensatz where
   (==) Nicht_im_Sortiment Nicht_im_Sortiment = True
   (==) _ _ = False

newtype Sortiment = Sort (Typ -> Datensatz)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq, Ord, Bounded, Enum, Show)

newtype Anbieter = A (Haendler -> Sortiment)

type Suchanfrage = Typ  

class Wgf a where                -- Wgf fuer `wohlgeformt'
 ist_wgf    :: a -> Bool         -- ist_wgf fuer `ist wohlgeformt'
 ist_nwgf   :: a -> Bool         -- ist_nwgf fuer `ist nicht wohlgeformt'
 wgf_fehler :: a -> a
 -- Protoimplementierungen
 ist_wgf x  = not (ist_nwgf x)
 ist_nwgf x = not (ist_wgf x)
 wgf_fehler = \x -> error "Argument fehlerhaft"


allLieferfenster :: [Lieferfenster]
allLieferfenster = [LF {quartal = x, jahr = y} | x <- enumFrom Q1, y <- enumFrom J2023 ]

allTypes :: [Typ]
allTypes = [M x | x <- enumFrom M1] ++ [T x | x <- enumFrom T1] ++ [S x | x <- enumFrom S1]


-- Aufgabe A.1

wg_la :: Lieferausblick -> [(Lieferfenster,Nat0)]
wg_la (LA f) = [(x, f x) | x <- allLieferfenster]

wg_so :: Sortiment -> [(Typ,Datensatz)]
wg_so (Sort f) = [(x, f x) | x <- allTypes]

wg_ab :: Anbieter ->  [(Haendler,Sortiment)]
wg_ab (A f) = [(x, f x) | x <- enumFrom H1]


{- Knapp, aber gut nachvollziehbar gehen die Implementierungen folgendermassen vor:
   ...
-}


-- Aufgabe A.2

instance Wgf Lieferausblick where
 ist_wgf la
   | lal == [] = True
   | otherwise = and (checkArray lal)
   where
      lal = wg_la la
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
 ist_wgf so = sortimentHelper $ sortOn fst sol
   where sol = wg_so so
 wgf_fehler _ = error "Sortimentfehler"

anbieterHelper :: [(Haendler, Sortiment)] -> Bool
anbieterHelper [] = True
anbieterHelper ((_, s):[]) = ist_wgf s
anbieterHelper ((f1, s1):(f2, s2):lf)
   | f1 /= f2 && ist_wgf s1 && ist_wgf s2 = anbieterHelper ((f2, s2):lf)
   | otherwise = False

instance Wgf Anbieter where
 ist_wgf a = anbieterHelper $ sortOn fst al
   where al = wg_ab a
 wgf_fehler a = error "Anbieterfehler"

{- Knapp, aber gut nachvollziehbar gehen die Instanzbildungen fuer Wgf folgendermassen vor:
   ...
-}

allHaendler :: Haendlerliste
allHaendler = reverse [minBound..maxBound]




-- Aufgabe A.5

type Haendlerliste = [Haendler]

checkAvailable :: Suchanfrage -> Anbieter -> Haendlerliste -> Haendlerliste
checkAvailable _ _ [] = []
checkAvailable s (A af) (lh:lt)
   | ds == Nicht_im_Sortiment = checkAvailable s (A af) lt
   | sofort_lieferbare_stueckzahl ds > 0 = lh : checkAvailable s (A af) lt
   | otherwise = checkAvailable s (A af) lt
   where (Sort sf) = af lh
         ds = sf s

sofort_lieferfaehig :: Suchanfrage -> Anbieter -> Haendlerliste
sofort_lieferfaehig t a
   | ist_wgf a = checkAvailable t a allHaendler
   | otherwise = error "Anbieterfehler"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.6

type Stueckzahl  = Nat0
type Gesamtpreis = Nat0

checkStockAndPrice :: Suchanfrage -> Anbieter -> Haendlerliste -> (Stueckzahl, Gesamtpreis) -> (Stueckzahl, Gesamtpreis)
checkStockAndPrice _ _ [] p = p
checkStockAndPrice s a@(A af) (lh:lt) p@(sz, gp)
   | ds == Nicht_im_Sortiment = checkStockAndPrice s a lt p
   | sofort_lieferbare_stueckzahl ds > 0 = checkStockAndPrice s a lt (sz + sofort_lieferbare_stueckzahl ds, gp + ((preis_in_euro ds) * sofort_lieferbare_stueckzahl ds))
   | otherwise = checkStockAndPrice s a lt p
   where (Sort sf) = af lh
         ds = sf s
 
sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Anbieter -> (Stueckzahl,Gesamtpreis)
sofort_erhaeltliche_Stueckzahl s a
   | ist_wgf a = checkStockAndPrice s a allHaendler (0, 0)
   | otherwise = error "Anbieterargumentfehler"

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.7

type Preis = EUR
guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Anbieter -> Maybe Haendlerliste
guenstigste_Lieferanten s lf a
   | ist_wgf a = finalValue
   | otherwise = error "Anbieterargumentfehler"
   where finalValue
            | prices == [] = Nothing
            | otherwise = Just (map fst (filter (\(a,b) -> b == minPrice) prices))
         prices = checkBestPrice s lf a allHaendler
         minPrice = minimum $ map snd prices

checkBestPrice :: Suchanfrage -> Lieferfenster -> Anbieter -> Haendlerliste -> [(Haendler, Nat1)]
checkBestPrice _ _ _ [] = []
checkBestPrice s lf a@(A af) (lh:lt)
   | ds == Nicht_im_Sortiment = checkBestPrice s lf a lt
   | laf lf > 0 = (lh, preis_in_euro ds) : checkBestPrice s lf a lt
   | otherwise = checkBestPrice s lf a lt
   where (Sort sf) = af lh
         ds = sf s
         (LA laf) = lieferbare_stueckzahl_im_Zeitfenster ds

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}


-- Aufgabe A.8

type RabattierterPreis = EUR

calculateRabbat :: Skonto -> Nat1 -> Nat0 -> RabattierterPreis
calculateRabbat Kein_Skonto p s = EUR {euro=((ceiling (fromIntegral(p*s)/10)) * 10)}
calculateRabbat DreiProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.97)/10))*10)}
calculateRabbat FuenfProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.95)/10))*10)}
calculateRabbat ZehnProzent p s = EUR {euro=((ceiling ((fromIntegral(p*s)*0.9)/10))*10)}

checkBestRabbatPrice :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> Haendlerliste -> [(Haendler, RabattierterPreis)]
checkBestRabbatPrice _ _ _ _ [] = []
checkBestRabbatPrice s lf sz a@(A af) (lh:lt)
   | ds == Nicht_im_Sortiment = checkBestRabbatPrice s lf sz a lt
   | laf lf >= sz = (lh, calculateRabbat (skonto ds) (preis_in_euro ds) sz) : checkBestRabbatPrice s lf sz a lt
   | otherwise = checkBestRabbatPrice s lf sz a lt
   where (Sort sf) = af lh
         ds = sf s
         (LA laf) = lieferbare_stueckzahl_im_Zeitfenster ds

guenstigste_Lieferanten_im_Lieferfenster :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Anbieter -> [(Haendler,RabattierterPreis)]
guenstigste_Lieferanten_im_Lieferfenster s lf sz a
   | ist_wgf a = filter (\(a, b) -> b == minPrice) prices
   | otherwise = error "Anbieterargumentfehler"
   where prices = checkBestRabbatPrice s lf sz a allHaendler
         minPrice = minimum $ map snd prices

{- Knapp, aber gut nachvollziehbar geht die Implementierung folgendermassen vor:
   ...
-}

