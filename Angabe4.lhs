> module Angabe4 where

1. Vervollst�ndigen Sie gemaess Angabentext!
2. Vervollst�ndigen Sie auch die vorgegebenen Kommentaranf�nge!
3. L�schen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisung!
4. Achten Sie darauf, dass `Gruppe' Leserechte f�r Ihre Abgabedatei hat!
5. Hinweis: Kommentar- und Programmzeilen m�ssen durch mindestens eine Leerzeile getrennt sein!


> type Nat0    = Int     -- Nat�rliche Zahlen beginnend mit 0
> type Nat1    = Int     -- Nat�rliche Zahlen beginnend mit 1
> type Nat2023 = Int     -- Nat�rliche Zahlen beginnend mit 2023

> newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Show, Ord)

> data Skonto  = Kein_Skonto 
>                | DreiProzent  
>                | FuenfProzent 
>                | ZehnProzent

> data Waschmaschinentyp   = WM_Typ1 | WM_Typ2 | WM_Typ3 | WM_Typ4 | WM_Typ5
> data Waeschetrocknertyp  = WT_Typ1 | WT_Typ2 | WT_Typ3 | WT_Typ4
> data Waescheschleudertyp = WS_Typ1 | WS_Typ2 | WS_Typ3

> data Typ = WM Waschmaschinentyp
>            | WT Waeschetrocknertyp
>            | WS Waescheschleudertyp

> data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
> type Jahr          = Nat2023
> data Lieferfenster = LF { quartal :: Quartal,
>                           jahr    :: Jahr 
>                         }

> data Datensatz 
>   = DS { preis_in_euro :: Nat1,
>          sofort_lieferbare_stueckzahl :: Nat0,
>          lieferbare_stueckzahl_im_Zeitfenster :: Lieferfenster -> Nat0,
>          skonto :: Skonto
>        }
>     | Nicht_im_Sortiment

> instance Eq Datensatz where
>   (==) Nicht_im_Sortiment Nicht_im_Sortiment = True
>   (==) _ _ = False

> data Sortiment 
>   = WMS {wm   :: Waschmaschinentyp   -> Datensatz}
>     | WTS {wt :: Waeschetrocknertyp  -> Datensatz}
>     | WSS {ws :: Waescheschleudertyp -> Datensatz}

> data Lieferantenname = L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 deriving(Eq, Bounded, Enum, Show)

> type Lieferanten = Lieferantenname -> Sortiment

> type Suchanfrage = Typ  


Aufgabe A.1

> type Lieferantenliste = [Lieferantenname]

> list :: Lieferantenliste
> list = [minBound..maxBound]

> loadDatensatz :: Sortiment -> Suchanfrage -> Datensatz
> loadDatensatz (WMS {wm=wm}) (WM x) = wm x
> loadDatensatz (WTS {wt=wt}) (WT x) = wt x
> loadDatensatz (WSS {ws=ws}) (WS x) = ws x
> loadDatensatz _ _ = Nicht_im_Sortiment

> checkAvailable :: Suchanfrage -> Lieferanten -> Lieferantenliste -> Lieferantenliste
> checkAvailable _ _ [] = []
> checkAvailable s l (lh:lt)
>   | x == Nicht_im_Sortiment = checkAvailable s l lt
>   | sofort_lieferbare_stueckzahl x > 0 = lh : checkAvailable s l lt
>   | otherwise = checkAvailable s l lt
>   where x = loadDatensatz (l lh) s


> sofort_erhaeltlich_bei :: Suchanfrage -> Lieferanten  -> Lieferantenliste
> sofort_erhaeltlich_bei s l = checkAvailable s l list

 Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...


Aufgabe A.2

> type Stueckzahl  = Nat0
> type Gesamtpreis = Nat0

> checkStockAndPrice :: Suchanfrage -> Lieferanten -> Lieferantenliste -> (Stueckzahl, Gesamtpreis) -> (Stueckzahl, Gesamtpreis)
> checkStockAndPrice _ _ [] p = p
> checkStockAndPrice s l (lh:lt) (sz, gp)
>   | x == Nicht_im_Sortiment = checkStockAndPrice s l lt (sz, gp)
>   | sofort_lieferbare_stueckzahl x > 0 = checkStockAndPrice s l lt (sz + sofort_lieferbare_stueckzahl x, gp + ((preis_in_euro x) * (sofort_lieferbare_stueckzahl x)))
>   | otherwise = checkStockAndPrice s l lt (sz, gp)
>   where x = loadDatensatz (l lh) s

> sofort_erhaeltliche_Stueckzahl :: Suchanfrage -> Lieferanten -> (Stueckzahl,Gesamtpreis)
> sofort_erhaeltliche_Stueckzahl s l = checkStockAndPrice s l list (0, 0)
        
Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
...
 

Aufgabe A.3

> type Preis = EUR

> checkBestPrice :: Suchanfrage -> Lieferfenster -> Lieferanten -> Lieferantenliste -> [(Lieferantenname, Nat1)]
> checkBestPrice _ _ _ [] = []
> checkBestPrice s lf l (lh:lt)
>   | x == Nicht_im_Sortiment = checkBestPrice s lf l lt
>   | lieferbare_stueckzahl_im_Zeitfenster x lf > 0 = (lh, preis_in_euro x) : checkBestPrice s lf l lt
>   | otherwise = checkBestPrice s lf l lt
>   where x = loadDatensatz (l lh) s

> guenstigste_Lieferanten :: Suchanfrage -> Lieferfenster -> Lieferanten -> Maybe Lieferantenliste
> guenstigste_Lieferanten s lf l
>   | prices == [] = Nothing
>   | otherwise = Just (map fst (filter (\(a,b) -> b == minPrice) prices))
>   where prices = checkBestPrice s lf l list
>         minPrice = minimum $ map snd prices

Knapp, aber gut nachvollziebar ,geht die Implementierung folgenderma�en vor:
... 


Aufgabe A.4

> type RabattierterPreis = EUR

> calculateRabbat :: Skonto -> Nat1 -> Nat0 -> RabattierterPreis
> calculateRabbat Kein_Skonto p s  = EUR {euro=(p*s)}
> calculateRabbat DreiProzent p s = EUR {euro=(ceiling (fromIntegral (p*s) * 0.97))}
> calculateRabbat FuenfProzent p s = EUR {euro=(ceiling (fromIntegral (p*s) * 0.95))}
> calculateRabbat ZehnProzent p s = EUR {euro=(ceiling (fromIntegral (p*s) * 0.9))}

> checkBestRabbatPrice :: Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> Lieferantenliste -> [(Lieferantenname, RabattierterPreis)]
> checkBestRabbatPrice _ _ _ _ [] = []
> checkBestRabbatPrice s lf sz l (lh:lt)
>   | x == Nicht_im_Sortiment = checkBestRabbatPrice s lf sz l lt
>   | lieferbare_stueckzahl_im_Zeitfenster x lf >= sz = (lh, calculateRabbat (skonto x) (preis_in_euro x) sz) : checkBestRabbatPrice s lf sz l lt
>   | otherwise = checkBestRabbatPrice s lf sz l lt
>   where x = loadDatensatz (l lh) s

> guenstigste_Lieferanten_im_Lieferfenster ::  Suchanfrage -> Lieferfenster -> Stueckzahl -> Lieferanten -> [(Lieferantenname,RabattierterPreis)]
> guenstigste_Lieferanten_im_Lieferfenster s lf sz l = filter (\(a,b) -> b == minPrice) prices
>   where prices = checkBestRabbatPrice s lf sz l list
>         minPrice = minimum $ map snd prices


Knapp, aber gut nachvollziebar, geht die Implementierung folgenderma�en vor:
... 
