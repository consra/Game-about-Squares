{-# OPTIONS_GHC -Wall #-}

module Levels where

import GAS

level0 :: Level
level0 = addObjects [ addCircle Red (2, 0)
                    , addSquare Red South (0, 0)
                    ]
                    emptyLevel

level1 :: Level
level1 = addObjects [ addCircle Blue (1, 0)
                    , addCircle Red (2, 0)
                    , addSquare Blue South (0, 0)
                    , addSquare Red North (3, 0)
                    ]
                    emptyLevel

level2 :: Level
level2 = addObjects [ addCircle Blue (0, 1)
                    , addCircle Red (0, 2)
                    , addCircle Gray (1, 1)
                    , addSquare Red East (0, 0)
                    , addSquare Blue North (2, 1)
                    , addSquare Gray West (1, 3)
                    ]
                    emptyLevel

level6 :: Level
level6 = addObjects [ addCircle Gray (0, 0)
                    , addCircle Blue (1, 1)
                    , addCircle Red (3, 2)
                    , addSquare Red South (0, 3)
                    , addSquare Blue West (1, 4)
                    , addSquare Gray North (2, 2)
                    ]
                    emptyLevel

level7 :: Level
level7 = addObjects [ addCircle Blue (0, 2)
                    , addArrow East (2, 0)
                    , addArrow North (2, 2)
                    , addSquare Blue South (0, 0)
                    ]
                    emptyLevel

addObjects :: [Level -> Level] -> Level -> Level
addObjects = foldl1 (.)
