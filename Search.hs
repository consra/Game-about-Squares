{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S
import Data.List as L
import Data.Maybe as Y
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = EmptyNode | CNode s (Maybe a) (Node s a) Int deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState (CNode stare _ _ _) = stare

unzip1::[(Int,Int)] -> ([Int], [Int])
unzip1 x = ((map fst x), (map snd x))

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}


compa ::(ProblemState s a, Ord s,Eq a)=> Node s a -> Node s a -> Ordering
compa (CNode stare1 _ _ _) (CNode stare2 _ _ _)
 	| (heuristic stare1) > (heuristic stare2) = GT
	| (heuristic stare1) < (heuristic stare2) = LT
	| otherwise = EQ

getNeighbours ::(ProblemState s a, Ord s,Eq a)=> Bool -> [Node s a] -> [Node s a]
getNeighbours bonus lista
	|bonus == False = lista
	|otherwise = sortBy compa lista

limitedDfs :: (ProblemState s a, Ord s,Eq a)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri

-- DFS inspirat din laborator
limitedDfs state bonus max_depth = helper [(CNode state Nothing EmptyNode 0)] max_depth S.empty []
	where
		 helper [] _ _ nodes = (reverse nodes)
		 helper (x:xs) depth visited nodes
		 	| node_depth > depth   = helper xs depth visited nodes
			| (S.member stare visited) == True = helper xs depth visited nodes
			| otherwise = helper (vecini++xs) depth (S.insert stare visited) (x:nodes)
				where
					(CNode stare _ _ node_depth) = x
					vecini = getNeighbours bonus (map (\(poz, star) -> (CNode star (Just poz) x (node_depth+1)) ) (successors stare))
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}
hasSolution :: (Eq s,Eq a) => Node s a -> [Node s a] -> [Node s a]-> (Node s a, Int)
hasSolution nod [] _ = (nod, -1)
hasSolution _ (x:_) lista_dfs = (x, total)
	where
		(CNode _ _ _ adancime) = x
		total = (Y.fromJust (elemIndex x lista_dfs) -1) + (length (extractPath x))

iterativeDeepening :: (ProblemState s a, Ord s,Eq a)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening s bonus = helper1 s 1
	where
		helper1 stare depth
			| (hasSolution dummy_nod lista_final lista_dfs) == (dummy_nod, -1) = helper1 stare (depth+3)
			| otherwise =  hasSolution dummy_nod lista_final lista_dfs
				where
					lista_dfs = (limitedDfs stare bonus depth)
					lista_final = (filter (\(CNode state _ _ _) -> (isGoal state)) lista_dfs)
					dummy_nod = (head lista_dfs)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
checkEmptyNode :: Node s a -> Bool
checkEmptyNode EmptyNode = True
checkEmptyNode (CNode _ _ _ _) = False

maybePair :: (Maybe a, s) -> (a, s)
maybePair (Just x, y) = (x, y)

convertMaybe :: [(Maybe a, s)] -> [(a, s)]
convertMaybe list1 = reverse (map maybePair list1)

extractPath :: Node s a -> [(a, s)]
extractPath nod = convertMaybe (unfoldr (\(CNode stare poz parinte _) -> if (checkEmptyNode parinte) then Nothing else Just(( poz, stare), parinte)) nod)

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
