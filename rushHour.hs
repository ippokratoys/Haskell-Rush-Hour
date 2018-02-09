import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

input ="\
\..abbb\n\
\c.adee\n\
\c..d==\n\
\.g.dhh\n\
\igjj.k\n\
\illl.k"


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
readFromStr (State sCars size1 size2) [] _ _ _= State (List.sort sCars) size1 size2

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
writeState s = printMap (lineSize s) (columnSize s) (stateToMap s (Map.empty)) 0 0

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

finalState s = if (y goalEndPoint)==colSize-1
    then True
    else False
    where
        goalCar = head $listOfCars s
        goalEndPoint = endPoint goalCar
        colSize = columnSize s
