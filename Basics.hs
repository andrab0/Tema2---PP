{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game
    { hunter :: Position
    , targets :: [Target]
    , obstacles :: [Position]
    , gateways :: [(Position, Position)]
    , nrLin :: Int
    , nrCol :: Int 
    } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

{-
	Tip de date pentru reprezentarea celulelor din tabla de joc.
-}

tip :: Position -> Game -> String
tip poziția (Game h t o g l c)
    | poziția == h = "!"
    | poziția `elem` (foldl(\acc x -> acc ++ [position x]) [] t) = "*"
    | poziția `elem` o = "@"
    | poziția `elem` (foldl(\acc x -> acc ++ [fst x]) [] g) = "#"
    | otherwise = " "


{-functAux :: Integer -> Integer -> [Position]
functAux col row = -}

gameAsString :: Game -> String
gameAsString joc@(Game h t o g l c) = 
	let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in
	foldl(\acc x -> if (snd x) == c-1 && (fst x) /= l-1 then  acc ++ (tip x joc) ++ "\n" else
		acc ++ (tip x joc)) "" lista


instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame nrLinii nrColoane = Game {
    hunter = (1, 1),
    targets = [],
    obstacles = completare_margini nrLinii nrColoane,
    gateways = [],
    nrLin = nrLinii,
    nrCol = nrColoane
}where completare_margini n m = [(0, i) | i <- [0..m-1]] ++ [(n-1, i) | i <- [0..m-1]] ++ [(i, 0) | i <- [0..n-1]] ++ [(i, m-1) | i <- [0..n-1]]


{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos joc@(Game h t o g l c)
    | tip pos joc /= " " = joc
    | let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in (pos `notElem` lista) = joc
    | otherwise = Game pos t o g l c 
{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav pos joc@(Game h t o g l c) = 
    Game h (t ++ [(Target pos behav)]) o g l c 

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) joc@(Game h t o g l c) =
    Game h t o (g ++ [(pos1, pos2), (pos2, pos1)]) l c

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos joc@(Game h t o g l c) = 
    Game h t (o ++ [pos]) g l c

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos joc@(Game h t o g l c)
    | tip pos joc == " " = Just pos
    | tip pos joc == "@" = Nothing
    | tip pos joc == "#" = Just (foldl(\acc x -> if (pos == fst x) then snd x else acc) (0,0) g)
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

goEast :: Behavior
goEast pos joc@(Game h t o g l c)
    | let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in ((fst pos, snd pos + 1) `notElem` lista) = (Target pos goEast)
	| tip (fst pos, snd pos + 1) joc == "@" = (Target pos goEast)
    | otherwise = (Target (fst pos, snd pos + 1) goEast)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos joc@(Game h t o g l c)
    | let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in ((fst pos, snd pos - 1) `notElem` lista) = (Target pos goWest)
    | tip (fst pos, snd pos - 1) joc == "@" = (Target pos goWest)
    | otherwise = (Target (fst pos, snd pos - 1) goWest)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos joc@(Game h t o g l c)
    | let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in ((fst pos - 1, snd pos) `notElem` lista) = (Target pos goNorth)
    | tip (fst pos-1, snd pos) joc == "@" = (Target pos goNorth)
    | otherwise = (Target (fst pos - 1, snd pos) goNorth)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos joc@(Game h t o g l c)
    | let lista = [(i,j) | i <- [0..l-1],
                         j <- [0..c-1]] in ((fst pos + 1, snd pos) `notElem` lista) = (Target pos goSouth)
    | tip (fst pos + 1, snd pos) joc == "@" = (Target pos goSouth)
    | otherwise = (Target (fst pos + 1, snd pos) goSouth)
{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce directie pos joc@(Game h t o g l c) = 
    if (directie == 1) then
        if (let lista = [(i, j) | i <- [0..l-1],
                                                j <- [0..c-1]] in ((fst pos + 1, snd pos) `notElem` lista)) then goNorth (fst pos - 1, snd pos) joc
        else 
            if ((tip (fst pos + 1, snd pos) joc) == "@") then goNorth (fst pos-1, snd pos) joc else goSouth pos joc
    else
        if (let lista = [(i, j) | i <- [0..l-1],
                                                j <- [0..c-1]] in ((fst pos - 1, snd pos) `notElem` lista)) then goSouth pos joc
        else
            if ((tip (fst pos - 1, snd pos) joc) == "@") then goSouth pos joc else goNorth pos joc



{-
    | (directie == 1) && (attemptMove (fst pos + 1, snd pos) joc == Nothing) = goNorth pos joc
    | (directie == 1) = goSouth pos joc
    | (directie == -1) && (attemptMove (fst pos - 1, snd pos) joc == Nothing) = goSouth pos joc
    | otherwise = goNorth pos joc
    -}

{-
    | (directie == 1) && let lista = [(i, j) | i <- [0..l-1],
                                                j <- [0..c-1]] in ((fst pos - 1, snd pos) `elem` lista) = goNorth pos joc
    | (directie == 1) && tip (fst pos - 1, snd pos) joc == "@" = goNorth pos joc
    | (directie == 1) = goSouth pos joc
    | (directie == -1) && let lista = [(i, j) | i <- [0..l-1],
                                                j <- [0..c-1]] in ((fst pos + 1, snd pos) `elem` lista) = goSouth pos joc
    | (directie == -1) && tip (fst pos + 1, snd pos) joc == "@" = goSouth pos joc
    | otherwise = goNorth pos joc 
-}
{-
bounce directie pos joc@(Game h t o g l c)
    | (directie == 1) && let lista = [(i, j) | i <- [0..l-1],
    	                      j <- [0..c-1]] in ((fst pos - 1, snd pos) `notElem` lista) = goNorth (fst pos - 1, snd pos) joc
    | (directie == 1) && tip (fst pos - 1, snd pos) joc == "@" = goNorth (fst pos - 1, snd pos) joc
    | (directie == 1) = goSouth pos joc
    | (directie == -1) && let lista = [(i, j) | i <- [0..l-1],
                               j <- [0..c-1]] in ((fst pos + 1, snd pos) `notElem` lista) = goSouth (fst pos + 1, snd pos) joc
    | (directie == -1) && tip (fst pos + 1, snd pos) joc == "@" = goSouth (fst pos + 1, snd pos) joc
    | otherwise = goNorth pos joc
-}




    -- if (directie == 1) then
    -- 	| let lista = [(i, j) | i <- [0..l-1],
    -- 	                      j <- [0..c-1]] in ((fst h + 1, snd h) `notElem` lista) = goNorth h joc
    --     | tip (fst h + 1, snd h) joc == "@" = goNorth h joc
    --     | otherwise = goSouth h joc
    -- else 
    --     | let lista = [(i, j) | i <- [0..l-1],
    --                            j <- [0..c-1]] in ((fst h - 1, snd h) `notElem` lista) = goSouth h joc
    --     | tip (fst h - 1, snd h) joc == "@" = goSouth h joc
    --     | otherwise = goNorth h joc 	                      

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets joc@(Game h t o g l c) = Game h (foldl (\acc x -> acc ++ [(behavior x) (position x) joc]) [] t) o g l c

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled pos targetul
    | (position targetul) == ((fst pos) - 1, snd pos) = True
    | (position targetul) == ((fst pos) + 1, snd pos) = True
    | (position targetul) == (fst pos, (snd pos) - 1) = True
    | (position targetul) == (fst pos, (snd pos) + 1) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState = undefined

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft joc@(Game h t o g l c)
    | null t = True
    | otherwise = False

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors = undefined

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal joc@(Game h t o g l c)  = foldl (\acc x -> if isTargetKilled h x then True else acc) False t 

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = undefined

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
