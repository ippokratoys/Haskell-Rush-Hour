-- A* Rush Hour implementation by
-- Thanasis Polydoros(1400164) & Vasileios Sioros(1500144)

import qualified Data.Set   as Set
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Map   as Map

--------------------------------------------------------------------------------
---------------------------------Examples---------------------------------------
--------------------------------------------------------------------------------

-- EASY
input = "\
\..abbb\n\
\..a.c.\n\
\..==c.\n\
\....d.\n\
\....d.\n\
\....d.\n"

-- MEDIUM
-- input = "\
-- \abccde\n\
-- \abffde\n\
-- \==.ghi\n\
-- \j..ghi\n\
-- \j..k..\n\
-- \.llk..\n"

-- HARD
-- input = "\
-- \aaabcd\n\
-- \effbcd\n\
-- \e.==cd\n\
-- \ggh...\n\
-- \.ih.jj\n\
-- \.ikkll\n"

printSolution s [] = putStrLn (writeState s)
printSolution s (m:ms) = do {
                                putStrLn (writeState s);
                                printSolution (makeMove s m) ms
                            }

example alg prob = printSolution initState (alg initState)
    where initState = readState prob

example1 = example solve input
example2 = example solve_astar input

--------------------------------------------------------------------------------
----------------------Rush Hour Data Structures / Methods-----------------------
--------------------------------------------------------------------------------

data Point = Point { x :: Int
                    ,y :: Int
                   } deriving (Show,Eq,Ord)

data Car = Car { idChar     :: Char
                ,startPoint :: Point
                ,endPoint   :: Point
               } deriving (Show,Eq)

instance Ord Car where
    compare (Car '=' _ _ ) (Car idChar2 _ _) = LT
    compare (Car idChar1 _ _) (Car '=' _ _) = GT
    compare (Car idChar1 _ _) (Car idChar2 _ _) = if (idChar1 == idChar2)
                                                  then EQ
                                                  else (if (idChar1 < idChar2) then LT else GT)

data State = State { listOfCars :: [Car]
                    ,lineSize   ::  Int
                    ,columnSize ::  Int
                   } deriving (Show,Eq)

data Move = Move { getCar    :: Car
                  ,getOffset :: Int
                 } deriving (Show, Eq)

-- gets a String and return the size of the board
getSize :: [Char] -> (Int,Int)
getSize s = ( 1+(length $ List.elemIndices '\n' s), if Maybe.isJust (columns) then Maybe.fromJust(columns) else (-1) )
    where columns = List.elemIndex '\n' s

readState s = readFromStr ( State [] (fst size) (snd size) ) s (Set.empty) 0 0
    where size = getSize s

-- s is the string
-- xs the input string
-- hist is a set with symbols that had already visited
-- current i
-- current j
readFromStr (State sCars size1 size2) [] _ _ _= State (List.sort sCars) (size1-1) size2

readFromStr curState (x:xs) hist i j
    | Set.member x hist = readFromStr curState xs hist (i) (j+1)
    | x=='\n' = readFromStr curState xs hist (i+1) (0)
    | x=='.' = readFromStr curState xs hist (i) (j+1)
    | otherwise = readFromStr newState xs newHist (i) (j+1)
        where
            frsPnt = Point i j
            horizontalLen = length (takeWhile (==x) xs)
            vericalLen = length (List.elemIndices x xs)
            newCar = if (horizontalLen/=0) then (Car x frsPnt (Point i $j+horizontalLen )  ) else (Car x frsPnt (Point (i+vericalLen) j)  )
            newState = State  ((listOfCars curState) ++ [newCar]) (lineSize curState) (columnSize curState)
            newHist = Set.insert x hist

-- gets a state and return an equal string
writeState s = (printMap (lineSize s) (columnSize s) (stateToMap s (Map.empty)) 0 0) ++ "\n"

-- gets a map of cars and prints it
printMap iMax jMax m i j
    | j == (jMax) = (if iMax-1==i then "" else "\n"++(printMap iMax jMax m (i+1) 0) )
    | otherwise = firstElem ++ (printMap iMax jMax m i $j+1)
    where
        res = Map.lookup (i,j) m
        firstElem = [if (Maybe.isJust res) then (Maybe.fromJust res) else '.']

-- stateToMap :: State -> Map.Map a b -> Map.Map a b
stateToMap (State [] size1 size2) curMap = curMap
stateToMap (State (x:xs) size1 size2) curMap = stateToMap (State xs size1 size2) newMap
    where
        newMap =  addCarToMap x curMap
        -- newMap = addCarToMap x curMap

-- takes a car, produces a list of the points that gets in the map and insert them to the map
addCarToMap curCar@(Car idCa (Point x1 y1) (Point x2 y2)) curMap = listToMap listOfPoints curMap
    where
        listOfPoints = [(x,idCa) | x<-pointsOfCar curCar]

-- add a list of elements to map
listToMap [] initVal = initVal
listToMap (x:xs) initVal = listToMap xs newVal
    where
        newVal = (Map.insert (fst x) (snd x) initVal)

-- return list of points that this car lives on
pointsOfCar (Car idCa (Point x1 y1) (Point x2 y2)) = if (x1==x2) then [ (x1,yy) | yy<-[y1..y2]] else [ (xx,y1) | xx<-[x1..x2] ]

finalState (State cars _ cols) = if horizontalPos == cols - 1 then True else False
    where horizontalPos = y $ endPoint $ head cars

makeMove (State cars rows cols) (Move car offset) = State ncars rows cols
    where point1  = startPoint car
          point2  = endPoint   car
          x1      = x point1
          y1      = y point1
          x2      = x point2
          y2      = y point2
          xdiff   = x1 - x2
          npoint1 = if xdiff == 0 then (Point x1 (y1 + offset)) else (Point (x1 + offset) y1)
          npoint2 = if xdiff == 0 then (Point x2 (y2 + offset)) else (Point (x2 + offset) y2)
          ncar    = (Car (idChar car) npoint1 npoint2)
          ncars   = List.insert ncar (List.delete car cars)

-- Recursively return the legal moves of every car
-- in the listOfCars of the given state
successorMoves s = successorMovesUtil s (listOfCars s)

successorMovesUtil _ []         = []
successorMovesUtil s (car:cars) = (legalMoves s car) ++ successorMovesUtil s cars

-- Return the legal moves of the specified car in both directions
-- if Rotation = Vertical then (up and down)
-- else (left and right)
legalMoves s car = (legalMovesUtil s car 0 (\x -> x + 1)) ++ legalMovesUtil s car 0 (\x -> x - 1)

-- Return the legal moves of the specified car in the given direction (next)
legalMovesUtil s car offset next
    | legalState (makeMove s nmove) = [(nmove, cost)] ++ legalMovesUtil s car noffset next
    | otherwise                     = []
    where noffset = next offset
          nmove   = Move car noffset
          cost    = 1

-- Check if any car is out of bounds or if any two cars intersect
legalState (State cars rows cols) = if flag1 || flag2 then False else True
    where flag1 = any (True==) (map (outOfBounds rows cols) cars)
          pairs = combos Set.empty [(car1, car2) | car1<-cars, car2<-(List.delete car1 cars)]
          flag2 = any (True==) (map (\(car1, car2)-> carIntersect car1 car2) pairs)

-- Discard any reversed tuples in order to save time as
-- car1 'intersects' car2 is the same as car2 'intersects' car1
combos _ [] = []
combos set ((car1, car2):ccs) = if Set.member (car2, car1) set
                                then combos set ccs
                                else (car1, car2):combos (Set.insert (car1, car2) set) ccs

-- Check given the size of the map if the car is out of bounds
outOfBounds rows cols (Car _ (Point x1 y1) (Point x2 y2))
    | x1 < 0 || y1 < 0 || x2 >= rows || y2 >= cols = True
    | otherwise                                    = False

-- Check if the two specified cars intersect in any way
carIntersect car1 car2
    | null (List.intersect points1 points2) = False
    | otherwise                             = True
    where points1 = pointsOfCar car1
          points2 = pointsOfCar car2

--------------------------------------------------------------------------------
------------------------Solve Data Structures / Methods-------------------------
--------------------------------------------------------------------------------

data Edge = Edge { move :: Move
                  ,cost :: Int
                 } deriving (Show, Eq)

data Node = Node { ancestor :: Maybe Node
                  ,edge     :: Maybe Edge
                  ,state    :: State
                  ,gScore   :: Int
                  ,fScore   :: Int
                 } deriving (Show, Eq)

instance Ord Node where
    compare na nb = if (fScore na) < (fScore nb) then LT else GT

solve initState       = solveBy initState (\n -> 0)

solve_astar initState = solveBy initState heuristic

-- Using the A* search algorithm solve the problem
-- specified by 'initState' using the heuristic function 'h'
solveBy initState h   = solveUtil closedSet openSet h
    where   start     = Node Nothing Nothing initState 0 (h initState)
            closedSet = Set.empty
            openSet   = Set.insert start Set.empty

solveUtil closedSet openSet h
    | Set.null openSet = []
    | finalState cs    = reconstruct_path cn
    | otherwise        = solveUtil nclosedSet nopenSet h
    where cn           = Set.findMin openSet
          cs           = state cn
          smcs         = map (\(m, c) -> (makeMove cs m, Edge m c)) (successorMoves cs)
          nclosedSet   = Set.insert cn closedSet
          nopenSet     = updtNeig (rmFromSet openSet cn) cn h (rmInSet nclosedSet smcs)

rmFromSet set val = Set.fromList $ List.delete val (Set.toList set)

rmInSet _ [] = []
rmInSet set (neig@(s, Edge m c):smcs) = if Maybe.isJust $ inSet set s
                                        then rmInSet set smcs
                                        else neig:(rmInSet set smcs)

updtNeig openSet cn _ [] = openSet
updtNeig openSet cn h (smc:smcs) = updtNeig os cn h smcs
    where os = updtNeigUtil openSet cn h smc

updtNeigUtil openSet cn h (s, Edge m c)
    | (Maybe.isJust pn) = if tgs < (gScore $ Maybe.fromJust pn)
                          then osa
                          else openSet
    | otherwise          = osb
    where pn  = inSet openSet s
          tgs = (gScore cn) + c
          jcn = Just cn
          je  = Just $ Edge m c
          un  = Node jcn je s tgs (tgs + (h s))
          osa = Set.insert un $ rmFromSet openSet (Maybe.fromJust pn)
          osb = Set.insert un openSet

inSet set s = List.find eqState lset
    where eqState = \n -> (state n) == s
          lset    = Set.toList set

reconstruct_path (Node Nothing Nothing _ _ _) = []
reconstruct_path (Node a e s g f) = (reconstruct_path ja) ++ [move je]
    where ja = Maybe.fromJust a
          je = Maybe.fromJust e

--------------------------------------------------------------------------------
------------------------------Heuristic Function--------------------------------
--------------------------------------------------------------------------------

heuristic s = (heuristicUtil s pntsRight)+(if ((length pntsRight)/=0) then 1 else 0)
    where
        goalStart   = startPoint $head $listOfCars s
        goalEnd     = endPoint $head $listOfCars s
        numLines    = lineSize s
        numCols     = columnSize s
        pntsOfGoal  = pointsOfCar $head $listOfCars s
        rightMost   = last pntsOfGoal
        pntsRight   = [ (i,j) | i<-[fst rightMost], j<-[((snd rightMost)+1)..(numCols-1) ] ]

heuristicUtil s [] = 0
heuristicUtil s (pnt:pnts)
    | Maybe.isJust carThere = max (1 + extraCost) (heuristicUtil s pnts)
    | otherwise = 0 + (heuristicUtil s pnts)
    where
        carThere  = findCar (fst pnt) (snd pnt) (listOfCars s)
        dataCar   = Maybe.fromJust carThere
        extraCost = canMove [] (Just dataCar) s

canMove _     Nothing    _ = 0
canMove seen (Just aCar) s
    | elem aCar seen = 0
    | isVertical     =
        if ((inBounds verticalBefore s && Maybe.isNothing carUp) || (inBounds verticalAfter s && Maybe.isNothing carDown))
        then 0
        else 1 + (min (canMove newSeen ( carUp) s) (canMove newSeen ( carDown) s))
    | otherwise      =
        if ((inBounds horizontalBefore s && Maybe.isNothing carLeft ) || ( inBounds horizontalAfter s && Maybe.isNothing carRight))
        then 0
        else 1 + (min (canMove newSeen ( carLeft) s) (canMove newSeen ( carRight) s))
    where
        isVertical         = (y $startPoint aCar) == (y $endPoint aCar)
        verticalBefore     = ((x $startPoint aCar)-1,y $startPoint aCar)
        verticalAfter      = (1+(x $endPoint aCar),y $endPoint aCar)
        horizontalBefore   = (x $startPoint aCar, (y $startPoint aCar)-1)
        horizontalAfter    = (x $endPoint aCar, 1+(y $endPoint aCar))
        carUp              = tryFindCar (fst verticalBefore) (snd verticalBefore) s
        carDown            = tryFindCar (fst verticalAfter) (snd verticalAfter) s
        carLeft            = tryFindCar (fst horizontalBefore) (snd horizontalBefore) s
        carRight           = tryFindCar (fst horizontalAfter) (snd horizontalAfter) s
        newSeen            = (aCar:seen)

tryFindCar i j s = if ( i < (lineSize s)) && (j < (columnSize s)) then findCar i j (listOfCars s) else Nothing

findCar i j ([]) = Nothing
findCar i j (fCar:tCars)
    |elem (i,j) (pointsOfCar fCar)  = Just fCar
    |otherwise                        = findCar i j (tCars)

isFree pnt s
    | outOfBounds (lineSize s) (columnSize s) fakeCar = False
    | Maybe.isJust carThere = False
    | otherwise = True
    where
        fakeCar  = Car '=' (Point (fst pnt) (snd pnt)) (Point (fst pnt) (snd pnt))
        carThere = findCar (fst pnt) (snd pnt) (listOfCars s)

inBounds (i,j) s = if ( i < (lineSize s)) && (j < (columnSize s)) then True else False

--------------------------------------------------------------------------------
----------------------------Pairing Heap Code--------------------------------------
--------------------------------------------------------------------------------

data HeapElem = EmpHeapElem | HeapElem{node :: Node} deriving (Show,Ord,Eq)

data PairingHeap = EmpHeap | PairingHeap { headOfHeap ::  HeapElem
                                          ,subheaps   :: [PairingHeap] } deriving (Show,Eq)

-- instance Ord HeapElem where
--     compare (HeapElem cost1 _) (HeapElem cost2 _) = compare cost1 cost2
--
-- instance Eq HeapElem where
--     (HeapElem _ st1) == (HeapElem _ st2) = st1==st2



findMin EmpHeap = EmpHeapElem
findMin (PairingHeap el _) = el


mergeHeap EmpHeap EmpHeap = EmpHeap
mergeHeap EmpHeap heap2 = heap2
mergeHeap heap1 EmpHeap = heap1
mergeHeap heap1 heap2 = PairingHeap (min minHeap1 minHeap2) ( [oldMax] ++subHeap1 ++ subHeap2)
    where
        minHeap1 = findMin heap1
        minHeap2 = findMin heap2
        subHeap1 = subheaps heap1
        subHeap2 = subheaps heap2
        oldMax   = PairingHeap (max minHeap1 minHeap2) []

insertHeap EmpHeap newElem = PairingHeap newElem []
insertHeap theHeap newElem = mergeHeap theHeap (PairingHeap newElem [])

mergeHeaps [] = EmpHeap
mergeHeaps [heap1] = heap1
mergeHeaps (heap1:heap2:heaps) = mergeHeap heap12 (mergeHeaps heaps)
    where
        heap12 = mergeHeap heap1 heap2


deleteMinHeap EmpHeap = EmpHeap
deleteMinHeap heap1 = mergeHeaps (subheaps heap1)

insetUpdateHeap EmpHeap _ = EmpHeap
insetUpdateHeap (PairingHeap headState subStates) delState = if (isSameNode (node headState) delState && isBetterNode delState (node headState))
    then (insertHeap (deleteMinHeap (PairingHeap headState subStates)) (HeapElem delState))
    else (if isDone
        then theUpdList
        else theInsList)
    where
        resOfDel    = updateElemHeap delState subStates
        isDone      = fst resOfDel
        theUpdList  = PairingHeap headState (snd resOfDel)
        theInsList  = insertHeap (PairingHeap headState subStates) (HeapElem delState)

updateElemHeap delState [] = (False,[])
updateElemHeap delState (st:sts) = if (isSameNode delState $node $headOfHeap st)
    then (ifBetter)
    else if ((fst resSub)==True) then (True,(snd resSub)++sts) else ((fst resTail),([st]++(snd resTail)))
    where
        newState = deleteMinHeap st
        updatedRes = if (newState)==EmpHeap then (True,sts) else (True,newState:sts)
        ifBetter = if (isBetterNode delState $node $headOfHeap st) then updatedRes else (True,st:sts)
        resTail = updateElemHeap delState sts
        resSub = updateElemHeap delState (subheaps st)
-- deleteElemHeap initHeap delState =

isSameNode s1 s2 = (state s1) == (state s2)
isBetterNode s1 s2 = fScore s1 < fScore s2
