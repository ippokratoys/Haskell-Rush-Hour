import qualified Data.Set   as Set
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Map   as Map

input ="\
\..abbb\n\
\c.adee\n\
\c..d==\n\
\.g.dhh\n\
\igjj.k\n\
\illl.k\n"


data Point = Point { x::Int
                    ,y::Int
                    } deriving (Show,Eq,Ord)

data Car = Car { idChar :: Char
               ,startPoint :: Point
               ,endPoint :: Point
               } deriving (Show,Eq)

data State = State { listOfCars :: [Car]
                    ,lineSize:: Int
                    ,columnSize::Int
                    } deriving (Show,Eq)

instance Ord Car where
    compare (Car '=' _ _ ) (Car idChar2 _ _) = LT
    compare (Car idChar1 _ _) (Car '=' _ _) = GT
    compare (Car idChar1 _ _) (Car idChar2 _ _) = if (idChar1==idChar2) then EQ else (if (idChar1<idChar2) then LT else GT)

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

data Move = Move {
    getCar    :: Car,
    getOffset :: Int
} deriving (Show, Eq)

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

successorMoves s = successorMovesUtil s (listOfCars s)

successorMovesUtil _ []         = []
successorMovesUtil s (car:cars) = (legalMoves s car) ++ successorMovesUtil s cars

legalMoves s car = (legalMovesUtil s car 0 (\x -> x + 1)) ++ legalMovesUtil s car 0 (\x -> x - 1)

legalMovesUtil s car offset next
    | legalState (makeMove s nmove) = [(nmove, cost)] ++ legalMovesUtil s car noffset next
    | otherwise                     = []
    where noffset = next offset
          nmove   = Move car noffset
          cost    = 1

-- Maybe add fromList toList in pairs ?
legalState (State cars rows cols) = if flag1 || flag2 then False else True
    where flag1 = any (True==) (map (outOfBounds rows cols) cars)
          pairs = [(car1, car2) | car1<-cars, car2<-(List.delete car1 cars)]
          flag2 = any (True==) (map (\(car1, car2)-> carIntersect car1 car2) pairs)

outOfBounds rows cols (Car _ (Point x1 y1) (Point x2 y2))
    | x1 < 0 || y1 < 0 || x2 >= rows || y2 >= cols = True
    | otherwise                                    = False

carIntersect car1 car2
    | null (List.intersect points1 points2) = False
    | otherwise                             = True
    where points1 = pointsOfCar car1
          points2 = pointsOfCar car2

finalState (State cars _ cols)
    | (Maybe.isJust res) && y (endPoint (Maybe.fromJust res)) == cols - 1 = True
    | otherwise                                                           = False
    where res = List.find (\car -> (idChar car) == '=') cars

data Edge = Edge {
    move :: Move,
    cost :: Int
} deriving (Show, Eq)

data Node = Node {
    ancestor :: Maybe Node,
    edge     :: Maybe Edge,
    state    :: State,
    gScore   :: Int,
    fScore   :: Int
} deriving (Show, Eq)

instance Ord Node where
    compare na nb = if (fScore na) < (fScore nb)
                    then LT
                    else GT

solve initState     = solveUtil closedSet openSet h
    where h         = \x -> 0
          start     = Node Nothing Nothing initState 0 (h initState)
          closedSet = Set.empty
          openSet   = Set.insert start Set.empty
        
solveUtil closedSet openSet h
    | Set.null openSet = []
    | finalState cs    = reconstruct_path cn
    | otherwise        = solveUtil nclosedSet nopenSet h
    where cn           = Maybe.fromJust $ Set.lookupMin openSet
          cs           = state cn
          smcs         = map (\(m, c) -> (makeMove cs m, Edge m c)) (successorMoves cs)
          nclosedSet   = Set.insert cn closedSet
          nopenSet     = updtNeig (rmFromSet openSet cn) cn h (rmInSet nclosedSet smcs)

-- Just for now till i figure smth better
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

-- DEBUGGING
printSolution s [] = putStrLn (writeState s)
printSolution s (m:ms) = do {
                                putStrLn (writeState s);
                                printSolution (makeMove s m) ms
                            }

main :: IO()
main = printSolution state (solve state)
    where state = readState "...a\n==.a\n....\n....\n"
    
------------------------ pairing heap code -------------------------------------

data HeapElem = EmpHeapElem | HeapElem{ cost::Int
                        , curState::State} deriving (Show)

data PairingHeap = EmpHeap | PairingHeap { headOfHeap :: HeapElem
                                        ,subheaps:: [PairingHeap] } deriving (Show)

instance Ord HeapElem where
    compare (HeapElem cost1 _) (HeapElem cost2 _) = compare cost1 cost2

instance Eq HeapElem where
    (HeapElem _ st1) == (HeapElem _ st2) = st1==st2



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
