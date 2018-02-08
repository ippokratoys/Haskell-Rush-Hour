import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

input ="\
\..abbb\n\
\c.adee\n\
\c==d..\n\
\.g.dhh\n\
\igjj.k\n\
\illl.k"


data Point = Point { x::Int
                    ,y::Int
                    } deriving (Show,Eq)

data Car = Car { idChar :: Char
               ,startPoint :: Point
               ,endPoint :: Point
               } deriving (Show,Eq)

data State = State { listOfCars :: [Car]
                    ,lineSize:: Int
                    ,columnSize::Int
                    } deriving (Show)

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
readFromStr curState [] _ _ _= curState

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
