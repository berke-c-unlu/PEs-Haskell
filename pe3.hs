module PE3 where

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)

-------------------------------------------------------------------------------------------
--------------------------------- DO NOT CHANGE ABOVE -------------------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------
-------------------------------------- PART I ---------------------------------------------

isInGrid :: Grid -> Coordinate -> Bool
isInGrid grid coor = if (gridWidth grid) > (fst coor) && (gridLen grid) > (snd coor) && fst coor>=0 && snd coor >=0 then True
                    else False

gridLen :: Grid-> Int
gridLen grid = length grid

gridWidth :: Grid->Int
gridWidth grid = length (grid !! 0)
                
-------------------------------------------------------------------------------------------

totalCount :: Grid -> Int
totalCount grid = allCount grid

isRock :: Cell -> Bool
isRock (Rock _) = True
isRock (_) = False

getRock:: Cell -> Int
getRock (Rock a) = a
getRock _ = 0


lineVal:: [Cell] -> Int
lineVal [] = 0
lineVal (x:xs) 
            | isRock x == True = getRock x + (lineVal xs)
            | otherwise = lineVal (xs)

allCount :: Grid -> Int
allCount [] = 0
allCount (cell:cells) = lineVal cell + allCount cells

-------------------------------------------------------------------------------------------

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits [] = []
coordinatesOfPits grid = sort (getPit grid 0 0) 0

isPit:: Cell -> Bool
isPit (Pit) = True
isPit _ = False

getPit :: Grid -> Int -> Int -> [Coordinate]
getPit [] _ _ = []
getPit grid x y
                | isPit ((grid !! y) !! x) == True = [(x,y)] ++ if isInGrid grid (x+1,y) then getPit grid (x+1) y
                                                                        else if isInGrid grid (0,y+1) then  getPit grid 0 (y+1)
                                                                        else []
                | otherwise = if isInGrid grid (x+1,y) then getPit grid (x+1) y
                                                                        else if isInGrid grid (0,y+1) then  getPit grid 0 (y+1)
                                                                        else []
                                                                        
bubble :: [Coordinate] -> [Coordinate]
bubble [] = []
bubble (x:[]) = [x]
bubble (x:y:[]) = if fst x > fst y then [y,x]
                  else [x,y]
bubble (first:second:other)
                            | fst first > fst second = [second] ++ bubble(first:other)
                            | otherwise = [first] ++ bubble(second:other)

sort [] _ = []
sort list times = if times == length list then list
                  else (sort (bubble list) (times+1))
-------------------------------------------------------------------------------------------

tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath grid robot moves = trace grid (location robot) moves (energy robot)

isNorth :: Move -> Bool
isNorth (North) = True
isNorth _ = False

isSouth :: Move -> Bool
isSouth (South) = True
isSouth _ = False

isWest :: Move -> Bool
isWest (West) = True
isWest _ = False

isEast :: Move -> Bool
isEast (East) = True
isEast _ = False

isPickUp :: Move -> Bool
isPickUp (PickUp) = True
isPickUp _ = False

isPutDown :: Move -> Bool
isPutDown (PutDown) = True
isPutDown _ = False

trace :: Grid -> Coordinate -> [Move] -> Int ->[Coordinate]
trace grid _ [] _  = []
trace grid loc (mov:movs) energy
                            | energy == 0 = loc:(trace grid loc movs 0)
                            | isPit ((grid !! snd loc) !! fst loc) = loc:(trace grid loc movs energy)
                            
                            
                            | isSouth mov && isInGrid grid (fst (loc),snd (loc)+1) && energy >=1 = (fst loc,(snd loc)+1):(trace grid (fst (loc),snd (loc)+1) movs (energy -1))
                            
                            | isNorth mov && isInGrid grid (fst (loc),snd (loc)-1)&& energy >=1 = (fst loc,(snd loc)-1):(trace grid (fst (loc),snd (loc)-1) movs (energy -1))
                            
                            | isWest mov && isInGrid grid (fst (loc)-1,snd (loc))&& energy >=1 = ((fst loc)-1,snd loc):(trace grid (fst (loc)-1,snd (loc)) movs (energy -1))
                            
                            | isEast mov && isInGrid grid (fst (loc)+1,snd (loc))&& energy >=1 = ((fst loc+1),snd loc):(trace grid (fst (loc)+1,snd (loc)) movs (energy -1))
                            
                            
                            
                            
                            | isSouth mov && isInGrid grid (fst (loc),snd (loc)+1) == False && energy >=1 = (fst (loc),snd (loc)):(trace grid (fst (loc),snd (loc)) movs (energy -1))
                            
                            | isNorth mov && isInGrid grid (fst (loc),snd (loc)-1) == False && energy >=1 = (fst (loc),snd (loc)):(trace grid (fst (loc),snd (loc)) movs (energy -1))
                            
                            | isWest mov && isInGrid grid (fst (loc)-1,snd (loc)) == False && energy >=1 = (fst (loc),snd (loc)):(trace grid (fst (loc),snd (loc)) movs (energy -1))
                            
                            | isEast mov && isInGrid grid (fst (loc)+1,snd (loc)) == False && energy >=1 = (fst (loc),snd (loc)):(trace grid (fst (loc),snd (loc)) movs (energy -1))
                            
                            
                            
                            | isPickUp mov = if energy < 5 then (loc):(trace grid (loc) movs (0))
                                             else (loc) : (trace grid (loc) movs (energy-5))
                            
                            | isPutDown mov = if energy < 3 then  (loc):(trace grid (loc) movs (0))
                                              else (loc) : (trace grid (loc) movs (energy-3))
                            
                            | otherwise = loc:(trace grid loc movs energy)
                            
------------------------------------- PART II ----------------------------------------------

energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots grid robots = energyHelper grid robots


energiseOneRobot :: Coordinate -> Coordinate -> Robot -> Robot
energiseOneRobot space rob robot = do
                                    let x = max 0 ( 100 - (abs(fst rob - fst space) + abs(snd rob - snd space))*20)
                                    let out = Robot { name = name robot,
                                                      location = location robot,
                                                      capacity = capacity robot,
                                                      energy = (if x + energy robot > 100 then 100 else x + energy robot) ,
                                                      storage = storage robot }
                                    out    
                                    
energyHelper :: Grid -> [Robot] -> [Robot]
energyHelper [] _ = []
energyHelper _ [] = []
energyHelper grid (x:xs) = [energiseOneRobot (findSpaceCraft grid 0 0) (location x) x] ++ energyHelper grid xs                                    
                                    
isCraft :: Cell -> Bool
isCraft (SpaceCraft a) = True
isCraft _ = False

getCraft :: Cell -> Int
getCraft (SpaceCraft a) = a
getCraft _ = 0


findSpaceCraft :: Grid -> Int -> Int -> Coordinate
findSpaceCraft grid x y
                        | isCraft ((grid !! y) !! x) == True = (x,y)
                        
                        | otherwise = if isInGrid grid (x+1,y) then findSpaceCraft grid (x+1) y
                                      else if isInGrid grid (0,y+1) then  findSpaceCraft grid 0 (y+1)
                                      else (-1,-1)
-------------------------------------------------------------------------------------------

applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot moves =  last (helper grid robot moves)

helper:: Grid -> Robot -> [Move] -> [(Grid, Robot)]
helper [] _ _ = []
helper _ _ [] = []
helper grid robot (mov:moves) = [x] ++ (helper (fst(x)) (snd(x)) moves)
                                where x =(applyOneMove grid robot mov)

applyOneMove:: Grid -> Robot -> Move -> (Grid,Robot)

applyOneMove grid robot move
                            | (energy robot) == 0 = (grid,robot)
                            
                            | isPit ((grid !! snd (location robot)) !! fst (location robot)) = do
                                                                                            let newRobot = Robot { name = name robot,
                                                                                                                  location = location robot,
                                                                                                                  capacity = capacity robot,
                                                                                                                  energy = (energy robot)-1,
                                                                                                                  storage = storage robot }
                                                                                            (grid,newRobot)

                            | isSouth move && (energy robot) >= 1 && isInGrid grid (fst ((location robot)),snd ((location robot))+1) = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = (fst ((location robot)),snd ((location robot))+1),
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                            
                            | isNorth move && (energy robot) >= 1 && isInGrid grid (fst ((location robot)),snd ((location robot))-1) = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = (fst ((location robot)),snd ((location robot))-1),
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                                                                                                                                        
                            | isWest move && (energy robot) >= 1 && isInGrid grid (fst ((location robot))-1,snd ((location robot))) = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = (fst ((location robot))-1,snd ((location robot))),
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                                                                                                                                        
                            | isEast move && (energy robot) >= 1 && isInGrid grid (fst ((location robot))+1,snd ((location robot))) = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = (fst ((location robot))+1,snd ((location robot))),
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)                                                                                                            
                            
                            
                            
                            
                            | isSouth move && (energy robot) >= 1 && isInGrid grid (fst ((location robot)),snd ((location robot))+1) == False = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = location robot,
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                            
                            | isNorth move && (energy robot) >= 1 && isInGrid grid (fst ((location robot)),snd ((location robot))-1) == False = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = location robot,
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                                                                                                                                        
                            | isWest move && (energy robot) >= 1 && isInGrid grid (fst ((location robot))-1,snd ((location robot))) == False = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = location robot,
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)
                                                                                                                                        
                            | isEast move && (energy robot) >= 1 && isInGrid grid (fst ((location robot))+1,snd ((location robot))) == False = do
                                                                                                                                        let newRobot = Robot { name = name robot,
                                                                                                                                                              location = location robot,
                                                                                                                                                              capacity = capacity robot,
                                                                                                                                                              energy = (energy robot)-1,
                                                                                                                                                              storage = storage robot }
                                                                                                                                        (grid,newRobot)                                                                                                            
                            
                            
                            | isPickUp move = 
                                              if energy robot <= 5 then do
                                                            let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot,
                                                                                  energy = 0,
                                                                                  storage = storage robot }
                                                            
                                                            (grid,newRobot)
                                              else if (storage robot) >= (capacity robot) then do
                                                            let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot,
                                                                                  energy = (energy robot)-5,
                                                                                  storage = storage robot }
                                                            
                                                            (grid,newRobot)   
                                              else if getRock ((grid !! snd (location robot))!! fst (location robot)) == 0 then do
                                                            let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot,
                                                                                  energy = (energy robot)-5,
                                                                                  storage = storage robot }
                                                            
                                                            (grid,newRobot)                  
                                              else do
                                                    let coor = (location robot)
                                                    let newNum = (getRock ((grid !! snd coor)!! fst coor)) -1
                                                    let newRock = (Rock newNum)
                                                    let newGrid = constructGrid grid coor newRock 0 0
                                                    let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot ,
                                                                                  energy = (energy robot)-5,
                                                                                  storage = storage robot +1 }
                                                            
                                                    (newGrid,newRobot)
                                                    
                            
                            | isPutDown move = if energy robot <= 3 then do
                                                            let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot,
                                                                                  energy = 0,
                                                                                  storage = storage robot }
                                                            
                                                            (grid,newRobot)
                                        
                                                else do
                                                    let coor = (location robot)
                                                    let newNum = (getCraft ((grid !! snd coor)!! fst coor)) +1
                                                    let newCraft = (SpaceCraft newNum)
                                                    let newGrid = constructGrid grid coor newCraft 0 0
                                                    let newRobot = Robot { name = name robot,
                                                                                  location = location robot,
                                                                                  capacity = capacity robot ,
                                                                                  energy = (energy robot)-3,
                                                                                  storage = storage robot -1 }
                                                            
                                                    (newGrid,newRobot)
                            
                            

constructGrid :: Grid -> Coordinate -> Cell -> Int -> Int ->Grid
constructGrid [] _ _ _ _ = []
constructGrid (a:ax) coor rock x y =
                                 if snd coor == y then [((take (fst coor) a) ++ [rock] ++ (drop(fst coor + 1) a))] ++ constructGrid ax coor rock x (y+1)
                                 else [a] ++ constructGrid ax coor rock x (y+1)

