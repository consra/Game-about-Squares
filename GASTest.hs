{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GASTest where

import GAS
import Levels
import ProblemState
import Search
import TestPP

import Control.Arrow (second)
import Control.Monad (foldM)

newtype Tree = Tree { getTree :: Int }
    deriving (Eq, Ord, Show)

instance ProblemState Tree Int where
    successors (Tree n)
        | n == 2    = [(1, Tree 4), (2, Tree 6)]  -- 4 instead of 5 => cycle
        | otherwise = [(1, Tree $ 2 * n + 1), (2, Tree $ 2 * n + 2)]

    isGoal (Tree n) = n == 13

testAddObject :: TestPP ()
testAddObject = tests 1 5
    [ testVal "addCircle.red.(0,0)" 1 "  r" $
          show $ addCircle Red (0, 0) emptyLevel
    , testVal "addCircle.blue.(17,22)" 1 "  b" $
          show $ addCircle Blue (17, 22) emptyLevel
    , testVal "addArrow.north.(0,0)" 1 "  ^" $
          show $ addArrow North (0, 0) emptyLevel
    , testVal "addArrow.west.(17,22)" 1 "  <" $
          show $ addArrow West (17, 22) emptyLevel
    , testVal "addSquare.red.north.(0,0)" 1 "R^ " $
          show $ addSquare Red North (0, 0) emptyLevel
    , testVal "addSquare.blue.west.(17,22)" 1 "B< " $
          show $ addSquare Blue West (0, 0) emptyLevel
    , testVal "addSquare.red.north.addArrow.north.(0,0)" 1 "R^^" $
          show $ addSquare Red North (0, 0) $ addArrow North (0, 0) emptyLevel
    , testVal "addSquare.red.north.addCircle.red.(17,22)" 1 "R^r" $
          show $ addSquare Red North (17, 22) $ addCircle Red (17, 22) emptyLevel
    ]

testShowLevels :: TestPP ()
testShowLevels = tests 2 15
    [ testVal "show.level0" 1 "Rv \n\
                              \   \n\
                              \  r" $ show level0
    , testVal "show.level1" 1 "Bv \n\
                              \  b\n\
                              \  r\n\
                              \R^ " $ show level1
    , testVal "show.level2" 1 "R> |  b|  r|   \n\
                              \   |  g|   |G< \n\
                              \   |B^ |   |   " $ show level2
    , testVal "show.level6" 1 "  g|   |   |Rv |   \n\
                              \   |  b|   |   |B< \n\
                              \   |   |G^ |   |   \n\
                              \   |   |  r|   |   " $ show level6
    , testVal "show.level7" 1 "Bv |   |  b\n\
                              \   |   |   \n\
                              \  >|   |  ^" $ show level7
    ]

testMove :: TestPP ()
testMove = tests 3 15
    -- no moves
    [ testVal "move.level0.(1,0)" 1 (show level0) $ show $ move (1, 0) level0
    , testVal "move.level0.(2,0)" 1 (show level0) $ show $ move (2, 0) level0
    -- move with no pushes
    , testVal "move.level0.1" 1 "Rv \n\
                                \  r" $ show $ movesLevel0 !! 1
    , testVal "move.level0.2" 1 "Rvr" $ show $ movesLevel0 !! 2
    , testVal "move.level0.3" 1 "  r\n\
                                \Rv " $ show $ movesLevel0 !! 3
    -- push 1 square
    , testVal "move.level1.2" 1 "  b\n\
                                \Bvr\n\
                                \R^ " $ show $ movesLevel1 !! 2
    , testVal "move.level1.3" 1 "  b\n\
                                \  r\n\
                                \Bv \n\
                                \R^ " $ show $ movesLevel1 !! 3
    , testVal "move.level1.4" 1 "  b\n\
                                \Bvr\n\
                                \R^ " $ show $ movesLevel1 !! 4
    -- push 2 squares
    , testVal "move.level6.3" 1 "  g|   |   |   \n\
                                \   |G^b|Rv |B< \n\
                                \   |   |   |   \n\
                                \   |   |  r|   " $ show $ movesLevel6 !! 3
    -- change heading
    , testVal "move.level7.2" 1 "   |   |  b\n\
                                \   |   |   \n\
                                \B>>|   |  ^" $ show $ movesLevel7 !! 2
    , testVal "move.level7.4" 1 "   |   |  b\n\
                                \   |   |   \n\
                                \  >|   |B^^" $ show $ movesLevel7 !! 4
    , testVal "move.level7.6" 1 "   |   |B^b\n\
                                \   |   |   \n\
                                \  >|   |  ^" $ show $ movesLevel7 !! 6
    ]
  where
    movesLevel0 = moveMany [(0, 0), (1, 0), (2, 0)] level0
    movesLevel1 = moveMany [(0, 0), (1, 0), (2, 0), (4, 0)] level1
    movesLevel6 = moveMany [(0, 3), (2, 2), (1, 4)] level6
    movesLevel7 = moveMany [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (1, 2)] level7

testLimitedDfs :: TestPP ()
testLimitedDfs = tests 4 15
    [ testVal "limitedDfs.Tree.0" 1 [0] $ listTo 0
    , testVal "limitedDfs.Tree.1" 1 [0, 1, 2] $ listTo 1
    , testVal "limitedDfs.Tree.2" 1 [0, 1, 3, 4, 2, 6] $ listTo 2
    , testVal "limitedDfs.Tree.3" 1 [0, 1, 3, 7, 8, 4, 9, 10, 2, 6, 13, 14] $
          listTo 3
    ]
  where
    listTo = map (getTree . nodeState) . limitedDfs (Tree 0) False

testIterativeDeepening :: TestPP ()
testIterativeDeepening = tests 5 10
    [ testVal "iterativeDeepening.Tree.state" 1 (Tree 13) $ nodeState node
    , testVal "iterativeDeepening.Tree.number" 1 20 $ number
    ]
  where
    (node, number) = iterativeDeepening (Tree 0) False

testExtractPath :: TestPP ()
testExtractPath = tests 6 10
    [ testVal "extractPath.Tree" 1 [(2, 2), (2, 6), (1, 13)] $
          map (second getTree) $ extractPath node
    ]
  where
    (node, _) = iterativeDeepening (Tree 0) False

testSuccessors :: TestPP ()
testSuccessors = tests 7 10
    [ testVal "successors.level0.1" 1 [((0, 0), "Rv \n\
                                                \  r")] $
          map (second show) $ successorsLevel01
    , testVal "successors.level0.2" 1 [((1,0),"Rvr")] $
          map (second show) $ successorsLevel02
    , testVal "successors.level1.1" 1 [((0,0),"Bvb\n\
                                              \  r\n\
                                              \R^ "),
                                       ((3,0),"Bv \n\
                                              \  b\n\
                                              \R^r")] $
          map (second show) $ successors level1
    ]
  where
    successorsLevel01 = successors level0
    successorsLevel02 = successors $ snd $ head successorsLevel01

testIsGoal :: TestPP ()
testIsGoal = tests 8 10
    [ testCond "isGoal.level0.0" 1 $ not $ isGoal $ movesLevel0 !! 0
    , testCond "isGoal.level0.1" 1 $ not $ isGoal $ movesLevel0 !! 1
    , testCond "isGoal.level0.2" 1 $ isGoal $ movesLevel0 !! 2
    , testCond "isGoal.level1.0" 1 $ not $ isGoal $ movesLevel1 !! 0
    , testCond "isGoal.level1.1" 1 $ not $ isGoal $ movesLevel1 !! 1
    , testCond "isGoal.level1.2" 1 $ isGoal $ movesLevel1 !! 2
    , testCond "isGoal.level7.0" 1 $ not $ isGoal $ movesLevel7 !! 0
    , testCond "isGoal.level7.2" 1 $ not $ isGoal $ movesLevel7 !! 2
    , testCond "isGoal.level7.4" 1 $ not $ isGoal $ movesLevel7 !! 4
    , testCond "isGoal.level7.6" 1 $ isGoal $ movesLevel7 !! 6
    ]
  where
    movesLevel0 = moveMany [(0, 0), (1, 0), (2, 0)] level0
    movesLevel1 = moveMany [(0, 0), (3, 0)] level1
    movesLevel7 = moveMany [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (1, 2)] level7

testSolve :: TestPP ()
testSolve = tests 9 10
    [ testCond "solve.level0" 1 $ isSolution level0 $ getSolution level0
    , testCond "solve.level1" 1 $ isSolution level1 $ getSolution level1
    , testCond "solve.level2" 1 $ isSolution level2 $ getSolution level2
    , testCond "solve.level6" 1 $ isSolution level6 $ getSolution level6
    , testCond "solve.level7" 1 $ isSolution level7 $ getSolution level7
    ]
  where
    getSolution = extractPath . fst . flip iterativeDeepening False

testHeuristic :: TestPP ()
testHeuristic = tests 10 20
    [ testCond "heuristic.level1" 1 $
        (isSolution level1 $ extractPath pH1) && nH1 < n1
    , testCond "heuristic.level2" 1 $
        (isSolution level2 $ extractPath pH2) && nH2 < n2
    , testCond "heuristic.level6" 1 $
        (isSolution level6 $ extractPath pH6) && nH6 < n6
    ]
  where 
    (_,    n1) = iterativeDeepening level1 False
    (pH1, nH1) = iterativeDeepening level1 True

    (_,    n2) = iterativeDeepening level2 False
    (pH2, nH2) = iterativeDeepening level2 True

    (_,    n6) = iterativeDeepening level6 False
    (pH6, nH6) = iterativeDeepening level6 True


moveMany :: [Position] -> Level -> [Level]
moveMany positions level = out
  where
    out = level : zipWith move positions out

isSolution :: Level -> [(Position, Level)] -> Bool
isSolution level path = isValidPath && null rest
  where
    isValidPath = foldM (\from (pos, to) ->
                             if move pos from == to then Just to else Nothing)
                        level path /= Nothing
    (_, _ : rest) = break (isGoal . snd) path

main :: IO ()
main = runTestPP $ sequence_ [ testAddObject
                             , testShowLevels
                             , testMove
                             , testLimitedDfs
                             , testIterativeDeepening
                             , testExtractPath
                             , testSuccessors
                             , testIsGoal
                             , testSolve
                             , testHeuristic
                             ]