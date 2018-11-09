{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState
import Debug.Trace(trace)
import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.

-}



data Square = CSquare Color Heading deriving (Eq, Ord)
data Circle = CCircle Color deriving (Eq, Ord)
data Arrow = CArrow  Heading deriving (Eq, Ord)

data Object = C1 Square | C2 Circle | C3 Arrow  deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
--de modificat
instance Show Object where
			show (C1 (CSquare square_color square_heading))
					| square_color == Red = "R" ++ show square_heading
					| square_color == Blue = "B" ++ show square_heading
					| otherwise = "G" ++ show square_heading
			show (C2 (CCircle circle))
					| circle == Red = "r"
					| circle == Blue = "b"
					| otherwise = "g"
    			show (C3 (CArrow arrow_heading)) = show arrow_heading


{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = CLevel (M.Map Position [Object]) deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}


findMaxColumn :: [(Int, Int)] -> Int
findMaxColumn [] = -1;
findMaxColumn [(_, _)] = 1;
findMaxColumn xs = (maximum (map snd xs) + 1)


findMaxLine :: [(Int, Int)] -> Int
findMaxLine [] = -1;
findMaxLine [(_, _)] = 1;
findMaxLine xs = (maximum (map fst xs) +1)

findMinColumn :: [(Int, Int)] -> Int
findMinColumn [] = -1;
findMinColumn [(_, _)] = 1;
findMinColumn xs = (minimum (map snd xs))

f x y z = x.y z

findMinLine :: [(Int, Int)] -> Int
findMinLine [] = -1;
findMinLine [(_, _)] = 1;
findMinLine xs = (minimum (map fst xs))

generateMatrixPosition :: Int -> Int -> Int -> Int -> [[(Int, Int)]]
generateMatrixPosition maxColumn maxLine minColum minLine = map (\line -> map (\column -> (line, column)) [minColum..(maxColumn-1)]) [minLine..(maxLine-1)]

printOjects :: [Object] -> String
--printOjects [] = ""
printOjects [(C3 a)] 		 = "  " ++ show (C3 a)
printOjects [(C2 a)]		 = "  " ++ show (C2 a)
printOjects [(C1 a)] 		 = show (C1 a) ++ " "
printOjects [(C1 a), (C2 b)] = show (C1 a) ++ show (C2 b)
printOjects [(C1 a), (C3 b)] = show (C1 a) ++ show (C3 b)

makeDecision :: (Int, Int) -> Level -> [(Int, Int)]-> [[(Int, Int)]] -> String
makeDecision (a, b) (CLevel nivel) line wholeMaxtrix =
										  if(M.member (a, b) nivel) then
											  if((last line) == (a, b) && (last wholeMaxtrix) == line) then
												 (printOjects (nivel M.! (a, b)))
											  else if((last line) == (a, b)) then
											  	 (printOjects (nivel M.! (a, b))) ++ "\n"
											  else
												 (printOjects (nivel M.! (a, b))) ++ "|"
										  else
											  if((last (last wholeMaxtrix)) == (a, b)) then
												  "   "
											  else if((last line) == (a, b)) then
												  "   \n"
											  else
												  "   |"

--(makeDecision per (CLevel nivel) line wholeMaxtrix))
instance Show Level where
    show (CLevel nivel)
		| (length (M.keys nivel)) > 1 = (concat (concat (map (\line ->  (map (\per -> (makeDecision per (CLevel nivel) line wholeMaxtrix)) line))  wholeMaxtrix)))
		|otherwise = (printOjects (nivel M.! (head (M.keys nivel))))
		where
				maxLine  	 = findMaxLine 	(M.keys nivel)
				maxColumn 	 = findMaxColumn (M.keys nivel)
				minLine  	 = findMinLine 	(M.keys nivel)
				minColumn 	 = findMinColumn (M.keys nivel)
				wholeMaxtrix = generateMatrixPosition maxColumn maxLine minColumn minLine

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = (CLevel M.empty)

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare cul dir (a, b) (CLevel nivel) = if (M.notMember (a, b) nivel)	then
										      (CLevel (M.insert (a,b) [(C1 (CSquare cul dir))] nivel))
										  else
											  (CLevel (M.insertWith (++) (a,b) [(C1 (CSquare cul dir))] nivel))
{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle cul (a, b) (CLevel nivel) = (CLevel (M.insert (a,b) [(C2 (CCircle cul))] nivel))

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h (a, b) (CLevel nivel) = (CLevel (M.insert (a,b) [(C3 (CArrow h))] nivel))

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

isThereSquare :: [Object] -> Bool
isThereSquare [(C3 _)] 		   = False
isThereSquare [(C2 _)] 		   = False
isThereSquare [(C1 _)] 		   = True
isThereSquare [(C1 _), (C2 _)] = True
isThereSquare [(C1 _), (C3 _)] = True

getInitialDirection :: [Object] -> Object
getInitialDirection [(C1 (CSquare _ heading))] = (C3 (CArrow heading))
getInitialDirection [(C1 _), (C3 a)] = (C3 a)
getInitialDirection [(C1 (CSquare _ heading)), (C2 _)] = (C3 (CArrow heading))

getNextPosition :: Object -> (Int, Int) -> (Int, Int)
getNextPosition (C3 (CArrow heading)) (a, b)
	|heading == North = (a-1, b)
	|heading == East = (a, b+1)
	|heading == West = (a, b-1)
	|otherwise = (a+1, b)

getNewSquare :: Object -> [Object] -> Object
getNewSquare (C1 a) [(C1 _)] = (C1 a)
getNewSquare (C1 a) [(C1 _), (C2 _)] = (C1 a)
getNewSquare (C1 (CSquare col _)) [(C1 _), (C3 (CArrow heading))] = (C1 (CSquare col heading))
getNewSquare (C1 (CSquare col _)) [(C3 (CArrow heading))]         = (C1 (CSquare col heading))
getNewSquare (C1 (CSquare col h)) [(C2 _)]         = (C1 (CSquare col h))

deleteSquare :: [Object]-> (Int, Int) -> Level -> Level
deleteSquare [(C2 _)] _ nivel 			= nivel
deleteSquare [(C3 _)] _ nivel 			= nivel
deleteSquare [(C1 _)] (a, b) (CLevel nivel) = (CLevel (M.delete (a, b) nivel))
deleteSquare [(C1 _), (C2 (CCircle col))] (a, b) (CLevel nivel) = addCircle col (a, b) (CLevel (M.delete (a, b) nivel))
deleteSquare [(C1 _), (C3 (CArrow heading))] (a, b) (CLevel nivel) = addArrow heading (a, b) (CLevel (M.delete (a, b) nivel))

moveSquare :: Object -> (Int, Int) -> Object -> Level -> Level
moveSquare (C1 (CSquare col heading)) (a,b) (C3 dir) (CLevel nivel)
   | (M.notMember (a, b) nivel) == True = addSquare col heading (a, b) (CLevel nivel)
   | (isThereSquare (nivel M.! (a, b))) == False = addSquare n_c n_h (a, b) (CLevel nivel)
   |otherwise = moveSquare curr_patrat nextPos (C3 dir) nou_nivel
	   where
		   curr_patrat = (head (nivel M.! (a, b)))
		   nextPos = getNextPosition (C3 dir) (a, b)
		   (C1 (CSquare n_c n_h)) = getNewSquare (C1 (CSquare col heading)) (nivel M.! (a, b))
		   nou_nivel = addSquare n_c n_h (a, b) (deleteSquare (nivel M.! (a, b)) (a, b) (CLevel nivel))


move :: Position -> Level -> Level
move (a,b) (CLevel nivel)
	| (M.notMember (a, b) nivel) == True = (CLevel nivel)
	| (isThereSquare (nivel M.! (a, b))) == False = (CLevel nivel)
	|otherwise = moveSquare patrat nextPos initialDirection nou_nivel
		where
			patrat = (head (nivel M.! (a, b)))
			initialDirection = getInitialDirection (nivel M.! (a, b))
			nextPos = getNextPosition initialDirection (a, b)
			nou_nivel = deleteSquare (nivel M.! (a, b)) (a, b) (CLevel nivel)
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}

isPositionRight :: [Object] -> Bool
isPositionRight [(C3 _)] 		   = True
isPositionRight [(C2 _)] 		   = True
isPositionRight [(C1 _)] 		   = False
isPositionRight [(C1 (CSquare c heading)), (C2 (CCircle col))] = c == col
isPositionRight [(C1 _), (C3 _)] = False

isSquareOnWritePosition :: [Object] -> Bool
isSquareOnWritePosition [(C3 _)] 		   = False
isSquareOnWritePosition [(C2 _)] 		   = False
isSquareOnWritePosition [(C1 _)] 		   = False
isSquareOnWritePosition [(C1 (CSquare c heading)), (C2 (CCircle col))] = c == col
isSquareOnWritePosition [(C1 _), (C3 _)] = False

getPositionsWithSquare :: Level -> [(Int, Int)]
getPositionsWithSquare (CLevel nivel) = filter (\(a,b) -> isThereSquare(nivel M.! (a, b))) poziti
	where
		poziti = M.keys nivel

instance ProblemState Level Position where
    successors (CLevel nivel) =  map (\poz ->  (poz,(move poz (CLevel nivel))) ) (getPositionsWithSquare (CLevel nivel))

    isGoal (CLevel nivel) = all isPositionRight (map snd (M.toList nivel))

    -- Doar petru BONUS
    heuristic (CLevel nivel) = ((length (getPositionsWithSquare (CLevel nivel))) - (length (filter isSquareOnWritePosition (map snd (M.toList nivel)))))
