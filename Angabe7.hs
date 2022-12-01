module Angabe7 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Vervollständigen Sie auch die vorgegebenen Kommentaranfänge!
   3. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   4. Achten Sie darauf, dass `Gruppe' Leserechte fuer Ihre Abgabedatei hat!
   5. Ersetzen Sie die Trivialimplementierungen error "Nicht implementiert" durch
      sinnvolle Implementierungen, die die jeweilige Aufgabenstellung erfüllen.
   6. Kopieren Sie Ihre Implementierungen von Angabe 3 bzw. 6 an den
      entsprechenden Stellen ein. Beachten Sie, dass dafür drei Umbennennungen
      erforderlich sind, um Namenskonflikte zwischen Bezeichnungen von
      Angabe 3 und 6 zu vermeiden.
-}


type Nat0    = Int     -- Natürliche Zahlen beginnend mit 0
type Nat1    = Int     -- Natürliche Zahlen beginnend mit 1
type Nat2023 = Int     -- Natürliche Zahlen beginnend mit 2023

newtype EUR  = EUR { euro :: Nat1 } deriving (Eq, Ord, Show)

data Skonto  = Kein_Skonto 
               | DreiProzent  
               | FuenfProzent 
               | ZehnProzent deriving (Eq, Show)

data Waschmaschine    = M1 | M2 | M3 | M4 | M5 deriving (Eq, Ord, Bounded, Enum, Show)
data Waeschetrockner  = T1 | T2 | T3 | T4 deriving (Eq, Ord, Bounded, Enum, Show)
data Waescheschleuder = S1 | S2 | S3 deriving (Eq, Ord, Bounded, Enum, Show)

data Typ = M Waschmaschine
           | T Waeschetrockner
           | S Waescheschleuder deriving (Eq, Ord, Show)

data Quartal       = Q1 | Q2 | Q3 | Q4 deriving (Eq,Ord,Show)
type Jahr          = Nat2023
data Lieferfenster = LF { quartal :: Quartal,
                          jahr    :: Jahr 
                        } deriving (Eq, Show)


instance Ord Lieferfenster where
    (LF q1 j1) `compare` (LF q2 j2)
        | j1 == j2 = q1 `compare` q2
        | otherwise = j1 `compare` j2

newtype Lieferausblick  = LA (Lieferfenster -> Nat0)
newtype Lieferausblick' = LA' [(Lieferfenster,Nat0)] deriving (Eq, Show)

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

data Datensatz' 
   = DS' { preis_in_euro' :: Nat1,
           sofort_lieferbare_stueckzahl' :: Nat0,
           lieferbare_stueckzahl_im_Zeitfenster' :: Lieferausblick',
           skonto' :: Skonto
        }
     | Nicht_im_Sortiment' deriving (Eq, Show)

newtype Sortiment  = Sort (Typ -> Datensatz)
newtype Sortiment' = Sort' [(Typ,Datensatz')] deriving (Eq, Show)

data Haendler = H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 deriving (Eq, Ord, Bounded, Enum, Show)

allHaendler :: [Haendler]
allHaendler = [minBound..maxBound]

allTypes :: [Typ]
allTypes = [M x | x <- enumFrom M1] ++ [T x | x <- enumFrom T1] ++ [S x | x <- enumFrom S1]

newtype Markt = Mt (Haendler -> Sortiment)
newtype Markt' = Mt' [(Haendler,Sortiment')] deriving (Eq, Show)

data Betroffen = Betroffen | NichtBetroffen deriving (Eq,Show)

newtype Betroffene_Haendler = BH (Haendler -> Betroffen)

type AbLieferfenster = Lieferfenster

ds'2ds :: Datensatz' -> Datensatz
ds'2ds Nicht_im_Sortiment' = Nicht_im_Sortiment
ds'2ds (DS' p s l sk) = (DS p s (lst2fkt_la' l) sk)


-- Aufgabe A.1

lst2fkt_la :: [(Lieferfenster,Nat0)] -> (Lieferfenster -> Nat0)
lst2fkt_la lst = \lf -> case lookup lf lst of
                          Just n -> n
                          Nothing -> error "undefiniert"

lst2fkt_so :: [(Typ,Datensatz')] -> (Typ -> Datensatz)
lst2fkt_so lst = \t -> case lookup t lst of
                         Just ds -> ds'2ds ds
                         Nothing -> error "undefiniert"

lst2fkt_ab :: [(Haendler,Sortiment')] -> (Haendler -> Sortiment)
lst2fkt_ab lst = \h -> case lookup h lst of
                         Just s -> lst2fkt_so' s
                         Nothing -> error "undefiniert"

{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.2

lst2fkt_la' :: Lieferausblick' -> Lieferausblick
lst2fkt_la' (LA' la) = LA (lst2fkt_la la)

lst2fkt_so' :: Sortiment' -> Sortiment
lst2fkt_so' (Sort' s) = Sort (lst2fkt_so s)

lst2fkt_ab' :: Markt' -> Markt
lst2fkt_ab' (Mt' m) = Mt (lst2fkt_ab m)


{- Knapp, aber gut nachvollziehbar, gehen die Implementierungen
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.4

transformSortiment :: Sortiment -> [Typ] -> [(Typ, Datensatz)]
transformSortiment _ [] = []
transformSortiment so@(Sort s) (th:tt) = (th, (s th)) : transformSortiment so tt 

transformMarkt :: Markt -> [Haendler] -> [(Haendler, [(Typ, Datensatz)])]
transformMarkt _ [] = []
transformMarkt ma@(Mt m) (hh:ht) = (hh, (transformSortiment (m hh) allTypes)) : transformMarkt ma ht

dsToMinDs :: Datensatz -> Int -> Datensatz
dsToMinDs Nicht_im_Sortiment _ = Nicht_im_Sortiment
dsToMinDs (DS p s l sk) np = (DS np s l sk)

coolFunction :: Datensatz -> Int
coolFunction Nicht_im_Sortiment = maxBound
coolFunction ds = preis_in_euro ds

handleIt :: [(Haendler, [(Typ, Datensatz)])] -> Typ -> Int
handleIt hl t = minimum $ map coolFunction $ map snd $ filter (\(a,b) -> (a == t)) $ concat $ map snd hl

handleHandleIt :: [(Haendler, [(Typ, Datensatz)])] -> [(Typ, Datensatz)] -> Typ -> Datensatz
handleHandleIt hl sl t = case lookup t sl of
                            Just d -> dsToMinDs d i
                            Nothing -> error "undefiniert"
    where i = handleIt hl t

sortToMinSort :: [(Haendler, [(Typ, Datensatz)])] -> [(Typ, Datensatz)] -> (Sortiment)
sortToMinSort hl sl = Sort (\t -> handleHandleIt hl sl t)



preisanpassung :: Markt -> Markt
preisanpassung m = Mt (\t -> case lookup t hl of
                                  Just s -> sortToMinSort hl s
                                  Nothing -> error "undefiniert")
    where hl = transformMarkt m allHaendler


{- Knapp, aber gut nachvollziehbar, geht die Implementierung
   Folgendermassen vor:
   ...
-}


-- Aufgabe A.5

handleBetroffenLieferAus :: Lieferausblick -> AbLieferfenster -> Lieferausblick
handleBetroffenLieferAus (LA l) al = LA (\t -> case t `compare` al of
                                                    LT -> l t
                                                    GT -> 0
                                                    EQ -> 0) 

handleBetroffenDatensatz :: Datensatz -> AbLieferfenster -> Datensatz
handleBetroffenDatensatz Nicht_im_Sortiment _ = Nicht_im_Sortiment
handleBetroffenDatensatz (DS p s l sk) al = (DS p s (handleBetroffenLieferAus l al) sk)

handleBetroffenSort :: Sortiment -> AbLieferfenster -> Sortiment
handleBetroffenSort (Sort s) al = Sort (\t -> handleBetroffenDatensatz (s t) al)

berichtige :: Markt -> Betroffene_Haendler -> AbLieferfenster -> Markt
berichtige ma@(Mt m) bh@(BH b) al = Mt (\t -> case b t of
                                                   Betroffen -> handleBetroffenSort (m t) al
                                                   NichtBetroffen -> m t)

{- Knapp, aber gut nachvollziehbar, geht die Implementierung
   Folgendermassen vor:
   ...
-}

