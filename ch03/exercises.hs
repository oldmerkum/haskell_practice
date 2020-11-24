import Data.List (sortBy,delete,sortOn,elemIndex)
import Data.Maybe
-- Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard
-- length function.
newLength :: [a] -> Int
newLength [] = 0
newLength (x:xs) = 1 + (newLength xs)

{-
Write a function that computes the mean of a list, i.e. the sum of all elements
in the list divided by its length. (You may need to use the fromIntegral
function to convert the length of the list from an integer into a floating
point number.)
-}
sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

avgList :: (Fractional a) => [a] -> a
avgList [] = 0
avgList xs = (sumList xs) / (fromIntegral (newLength xs))

{-
Turn a list into a palindrome, i.e. it should read the same both backwards and
forwards. For example, given the list [1,2,3], your function should return
[1,2,3,3,2,1].
-}
toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

-- 	Write a function that determines whether its input list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome (x:xs) = if x == last xs
    then isPalindrome (init xs)
    else False
isPalindrome [] = True

{-
Create a function that sorts a list of lists based on the length of each
 sublist. (You may want to look at the sortBy function from the Data.List
  module.)
-}
-- import Data.List (sortBy) required in beginning of file
compareListLen :: [a] -> [a] -> Ordering
compareListLen a b = compare (length a) (length b)

lenListofListSort :: [[a]] -> [[a]]
lenListofListSort xofxs = sortBy compareListLen xofxs

{-
Define a function that joins a list of lists together using a separator value.
No comments

-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]

The separator should appear between elements of the list, but should not follow
the last element.
-}
intersperse :: a -> [[a]] ->[a]
intersperse _ [] = []
intersperse _ (list:[]) = list
intersperse sep (list:lists) = list ++ sep:(intersperse sep lists)

{-
Using the binary tree type that we defined earlier in this chapter, write a
function that will determine the height of the tree. The height is the largest
number of hops from the root to an Empty. For example, the tree Empty has height
 zero; Node "x" Empty Empty has height one;
  Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
-}
data Tree a = Node {
    value       :: a,
    leftChild   :: (Tree a),
    rightChild  :: (Tree a)
    }
    | Empty
    deriving (Show)

testTree = Node "root" Empty Empty

treeHeight :: Tree tree -> Int
treeHeight Empty = 0
treeHeight tree = 1 + (max leftHeight rightHeight)
    where
        leftHeight = treeHeight (leftChild tree)
        rightHeight = treeHeight (rightChild tree)

{-
Consider three two-dimensional points a, b, and c. If we look at the angle
 formed by the line segment from a to b and the line segment from b to c, it
 either turns left, turns right, or forms a straight line. Define a Direction
 data type that lets you represent these possibilities.
-}
data Direction = LeftTurn
               | RightTurn
               | Straight
               deriving (Show, Eq)
{-
Write a function that calculates the turn made by three 2D points and returns
 a Direction.
Note
P1 = (x1, y1)
P2 = (x2, y2)
P3 = (x3, y3)
Direction = (x2 - x1)(y3 - y1) - (y2 - y1)(x3 - x1)
Direction = 0 means points are collinear (on the same line)
Direction > 0 means left turn
Direction < 0 means right turn
-}
type Point2D = (Double, Double)

findTurnDirection :: Point2D -> Point2D -> Point2D -> Direction
findTurnDirection p1 p2 p3
    | result == 0   = Straight
    | result > 0    = LeftTurn
    | result < 0    = RightTurn
    where
        result = ((fst p2 - fst p1) * (snd p3 - snd p1)) -
            ((snd p2 - snd p1) * (fst p3 - fst p1))

{-
Define a function that takes a list of 2D points and computes the direction of
 each successive triple. Given a list of points [a,b,c,d,e], it should begin by
 computing the turn made by [a,b,c], then the turn made by [b,c,d],
 then [c,d,e]. Your function should return a list of Direction.
-}
getDirectionList :: [Point2D] -> [Direction]
getDirectionList points
    | length points < 3 = []
    | otherwise         =
        first3PointsDir:getDirectionList (tail points)
    where
        first3Points = take 3 points
        first3PointsDir = findTurnDirection (head first3Points)
            (first3Points !! 1) (last first3Points)

pointList = [(1.0,0.0),(1.0,0.0),(1.0,0.0),(0.0,0.0),(0.0,1.0),((-1.0),0.0)]

{-
Using the code from the preceding three exercises, implement Graham's scan
 algorithm for the convex hull of a set of 2D points. You can find good
 description of what a convex hull. is, and how the Graham scan algorithm
 should work, on Wikipedia.

 Algorithm steps
 1. find the point with the lowest y-coordinate
    a. if more than one lowest y-coordinate point exists, then out of them use
    the one with the lowest x-coordinate as well. call this point point_p.
    b. takes O(n) time.
 2. Next, the set of points must be sorted in increasing order of the angle they
    and the point_p make with the x-axis.
    a. Sorting in order of angle does not require computing the angle.

-}
testPoints = [(2.0,1.0), (-2.0,0), (0,-2.0), (0.5, 0), (0,0), (-2,1)]
testPoints2 = [(0.0, 3.0), (1, 1), (2, 2), (4, 4), (0, 0), (1, 2), (3, 1), (3, 3)]

polarAngle :: Point2D -> Point2D -> Double
polarAngle other_point anchor_point = atan2 y_span x_span
    where
        y_span = (snd other_point) - (snd anchor_point)
        x_span = (fst other_point) - (fst anchor_point)

comparePolarAngles :: Point2D -> Point2D -> Point2D -> Ordering
comparePolarAngles anchor a b = compare (polarAngle a anchor)
    (polarAngle b anchor)

compareYValue :: Point2D -> Point2D -> Ordering
compareYValue a b = compare (snd a) (snd b)

compareXValue :: Point2D -> Point2D -> Ordering
compareXValue a b = compare (fst a) (fst b)

sortPoint2DListByYValue :: [Point2D] -> [Point2D]
sortPoint2DListByYValue points = sortBy compareYValue points

findAnchor :: [Point2D] -> Point2D
findAnchor points =
    if (snd a) == (snd b) && (fst a) > (fst b)
    then findAnchor (b:other_points)
    else a
    where
        a:b:other_points = sortPoint2DListByYValue points

sortOnPolar :: [Point2D] -> [Point2D]
sortOnPolar points = anchor : sortOn polar other_points
    where
        polar point = polarAngle anchor point
        anchor = findAnchor points
        other_points = delete anchor points

deleteRightTurn :: [(Point2D,Direction)] -> [Point2D]
deleteRightTurn [] = []
deleteRightTurn (x:xs) =
    if snd x == RightTurn
    then fst (unzip xs)
    else (fst x):(deleteRightTurn xs)

grahamScan :: [Point2D] -> [Point2D]
grahamScan [] = []
grahamScan points = if elem RightTurn (getDirectionList points)
                    then grahamScan lessRTurnPts
                    else points
                    where
                        dirList = getDirectionList (points ++ [head points])
                        trimPts = tail points
                        lessRTurnPts = (head points):
                            (deleteRightTurn (zip trimPts dirList))

getConvexHull :: [Point2D] -> [Point2D]
getConvexHull [] = []
getConvexHull points = grahamScan (sortOnPolar points)
