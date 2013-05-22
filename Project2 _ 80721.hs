-- Валерия Петкова Браянова
-- ф.н. 80721
-- група 5
-- Тип на корена а :: Char

import System.IO
-- DATA TREE
data Tree rt =
	Empty |
	Node {
		root :: rt , 
		leftTree :: Tree rt , 
		rightTree :: Tree rt
	}		
	deriving (Eq, Read, Show)
--end


-- zad1
-- Creates a tree 
makeTree :: a -> Tree a -> Tree a -> Tree a
makeTree x Empty Empty = Node x Empty Empty
makeTree x Empty (Node b lTree2 rTree2) = 
		Node x Empty (Node b lTree2 rTree2)
makeTree x (Node a lTree1 rTree1) Empty = 
		Node x (Node a lTree1 rTree1) Empty
makeTree x (Node a lTree1 rTree1) (Node b lTree2 rTree2) = 
		Node x (Node a lTree1 rTree1) (Node b lTree2 rTree2)
	
-- Creates a leaf	
makeLeaf :: a -> Tree a
makeLeaf x = Node {root = x, leftTree = Empty, rightTree = Empty}

-- get root Tree
getRootTree :: Tree a -> a
getRootTree (Node r lT rT) = r

-- get left Tree
getLeftTree :: Tree a -> Tree a
getLeftTree Empty = Empty
getLeftTree (Node r lT rT) = lT

-- get right Tree
getRightTree :: Tree a -> Tree a
getRightTree Empty = Empty
getRightTree (Node r lT rT) = rT

-- Zad. 2
parseTree::String->IO (Tree Char)
parseTree filename = do
	input <- readFile filename
	return (parseTreeFromString (init input))

-- returns String with right subTree
getListForRightTree :: String -> Int -> String
getListForRightTree str counter
 | counter == 0 = (tail str)
 | (head str) == '(' = getListForRightTree (tail str) (counter + 1)
 | (head str) == ')' = getListForRightTree (tail str) (counter - 1)
 | otherwise = getListForRightTree (tail str) counter

-- returns String with left subTree
getListForLeftTree :: String -> Int -> String -> String
getListForLeftTree str counter result
 | counter == 0 = result
 | (head str) == '(' = getListForLeftTree (tail str) (counter + 1) (result ++ [(head str)])
 | (head str) == ')' = getListForLeftTree (tail str) (counter - 1) (result ++ [(head str)])
 | otherwise = getListForLeftTree (tail str) counter (result ++ [(head str)])

-- makes Tree from String
parseTreeFromString :: String -> (Tree Char)
parseTreeFromString "()" = Empty
parseTreeFromString str = 
	makeTree (head (tail str)) 
			 (parseTreeFromString (getListForLeftTree (drop 4 str) 1 ['('])) 
			 (parseTreeFromString (getListForRightTree (init (drop 4 str)) 1 ))


--start zad3
-- tree compare
compareNodes :: (Eq a) => Tree a -> Tree a -> Bool
compareNodes Empty Empty = True
compareNodes (Node _ lT1 rT1) Empty = False
compareNodes Empty (Node r2 lT2 rT2) = False
compareNodes (Node _ lT1 rT1) (Node _ lT2 rT2) = 
	(compareNodes lT1 rT2) && (compareNodes rT1 lT2)


-- is tree symmetric
isSymmetric :: (Eq a) => Tree a -> Bool
isSymmetric Empty = True
isSymmetric (Node r leftT rightT) = 
	compareNodes leftT rightT	

-- zad4
-- makes Full Char BinTree with depth m
makeFullTree :: Int -> Tree Char
makeFullTree m
 | m == 0 = Empty
 | otherwise = makeTree 'a' (makeFullTree (m-1)) (makeFullTree (m-1)) 
	
-- get max height of Node
maxHeightNode :: Tree a -> Int
maxHeightNode Empty = 0
maxHeightNode (Node rt lT rT) = 1 + (max (maxHeightNode lT) (maxHeightNode rT)) 

-- checks if Tree is balanced 
isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node rt lT rT) = 
	if abs((maxHeightNode lT) - (maxHeightNode rT)) > 1  then False
	else
		(isBalanced lT) && (isBalanced rT)
	
-- adds one leaf to list of trees in all possible ways
add1VerticeAnywhere :: Tree Char -> [Tree Char]
add1VerticeAnywhere (Node rt Empty Empty) = [Node rt (makeLeaf 'а') Empty, Node rt Empty (makeLeaf 'а')]
add1VerticeAnywhere (Node rt Empty r) = [Node rt (makeLeaf 'а') r] ++ map (Node rt Empty)(add1VerticeAnywhere r)
add1VerticeAnywhere (Node rt l Empty) = map (\x -> (Node rt x Empty)) (add1VerticeAnywhere l) ++ [Node rt l (makeLeaf 'а')]
add1VerticeAnywhere (Node rt l r) = map (\x -> (Node rt x r)) (add1VerticeAnywhere l) ++ map (Node rt l) (add1VerticeAnywhere r)

-- returns list of unique elements of given list
unique :: (Eq a) => [Tree a] -> [Tree a]
unique [] = []
unique (x:xs) = x : unique (filter (\elem -> x /= elem) xs)

-- returns list of all unique trees with added 1 vertice
extendTrees :: [Tree Char] -> [Tree Char] 
extendTrees trees = unique (foldr (++) [] (map add1VerticeAnywhere trees))

-- returns list of all trees with added N vertices
getAllTreesWithNVertices :: Int -> [Tree Char] -> [Tree Char]
getAllTreesWithNVertices n trees
 | n == 0 = trees
 | otherwise = getAllTreesWithNVertices (n - 1) (extendTrees trees)   
 
-- get m, where m = max ( 2^m - 1 <= n )
getM :: Int -> Int
getM n = floor (logBase 2 (fromIntegral n))
 
allSymmetricAndBalanced :: Int -> [Tree Char]
allSymmetricAndBalanced n 
 | n <= 0 = []
 | otherwise = 
	filter (isSymmetric) 
		   (filter (isBalanced) 
				   (getAllTreesWithNVertices (n - 2^(getM n) + 1) [makeFullTree (getM n)])) 
	

-- zad 5

-- gets Maximum height on right side
getMaxRightHeight :: Tree Char -> Int
getMaxRightHeight Empty = 0
getMaxRightHeight (Node r lT rT)
 | rT == Empty = getMaxRightHeight lT
 | otherwise = (getMaxRightHeight lT) + (getMaxRightHeight rT) + 1
 
-- gets Maximum height on left side
getMaxLeftHeight :: Tree Char -> Int
getMaxLeftHeight Empty = 0
getMaxLeftHeight (Node r lT rT) 
 | lT == Empty = getMaxLeftHeight rT
 | otherwise = (getMaxLeftHeight lT) + (getMaxLeftHeight rT) + 1

-- makes Coordinates List with roots
helpMakeCoordList :: Tree Char -> Int -> Int -> [(Char, Int, Int)]
helpMakeCoordList Empty _ _ = []
helpMakeCoordList (Node r lT rT) x y = 
	(if rT /= Empty then [(getRootTree rT, x, y + 1 +(getMaxRightHeight lT))] else [])
	++ 
    (if lT /= Empty then [(getRootTree lT, x + 1 + (getMaxLeftHeight rT), y)] else [])
	++
    (helpMakeCoordList lT (x + 1 + (getMaxLeftHeight rT)) y) ++
    (helpMakeCoordList rT x (y + 1 + (getMaxRightHeight lT)))


-- appends root to coordinates list									
makeCoordList :: Tree Char -> [(Char, Int, Int)]
makeCoordList Empty = []
makeCoordList tree = [(getRootTree tree, 0, 0)] ++ (helpMakeCoordList tree 0 0)  
		
-- help functions for first, second and third element of triplets
first (a, _, _) = a
second (_, b, _) = b
third (_, _, c) = c

-- checks if there is vertex at position
hasAt :: Tree Char -> Int -> Int -> Bool
hasAt Empty _ _ = False
hasAt tree x y = (not (null (filter (\el -> x == (second el) && y == (third el)) (makeCoordList tree))))

-- get vertex at
getAt :: Tree Char -> Int -> Int -> Char
getAt tree x y = if hasAt tree x y then first(head(filter (\el -> x == (second el) && y == (third el)) (makeCoordList tree)))
				 else ' '

-- checks if there is vertex above
hasUp :: Tree Char -> Int -> Int -> Bool
hasUp Empty _ _ = False
hasUp tree x y = (not (null (filter (\el -> x > (second el) && y == (third el)) (makeCoordList tree))))

-- checks if there is vertex beneath
hasDown :: Tree Char -> Int -> Int -> Bool
hasDown Empty _ _ = False
hasDown tree x y = (not (null (filter (\el -> x < (second el) && y == (third el)) (makeCoordList tree))))

-- checks if there is vertex left
hasLeft :: Tree Char -> Int -> Int -> Bool
hasLeft Empty _ _ = False
hasLeft tree x y = (not (null (filter (\el -> x == (second el) && y > (third el)) (makeCoordList tree))))

-- checks if there is vertex right
hasRight :: Tree Char -> Int -> Int -> Bool
hasRight Empty _ _ = False
hasRight tree x y = (not (null (filter (\el -> x == (second el) && y < (third el)) (makeCoordList tree))))

-- prints link line string
makeLinkLine :: Tree Char -> Int -> String -> Int -> String
makeLinkLine tree x str y =
	if y >= (1 + (getMaxRightHeight tree)) then str
	else
		makeLinkLine tree x (str ++ (if (hasDown tree x y) && ((hasUp tree x y) || (hasAt tree x y))
										then "  | "
									else "    " ))  
							(y + 1)

-- prints line string
makeLine :: Tree Char -> Int -> String -> Int -> String
makeLine tree x str y =
	if y >= (1 + (getMaxRightHeight tree)) then str
	else
		makeLine tree x (str ++ 
						(if (hasLeft tree x y) && (hasRight tree x y)
							then if hasAt tree x y 
									then "-(" ++ [(getAt tree x y)] ++ ")"
								 else "----"
						 else if (hasUp tree x y) && (hasDown tree x y)
								 then if hasAt tree x y 
										 then " (" ++ [getAt tree x y] ++ ")" 
									  else "  | "
							  else if (hasAt tree x y) 
									   then if hasLeft tree x y 
												then "-(" ++ [getAt tree x y] ++ ")"
											else " (" ++ [getAt tree x y] ++ ")"
								   else "    "))  
						(y + 1)
	
-- prints the Tree
printTree :: Tree Char -> Int -> String
printTree Empty _ = ""
printTree tree count 
 | count > (getMaxLeftHeight tree) = ""
 | otherwise = let 
			str = (if hasAt tree count 0
						then "(" ++ [(getAt tree count 0)] ++ ")"
					else if hasDown tree count 0 then " | " 
						else "   ")
			strLink = (if hasDown tree count 0 then " | "
						else "   ")
			in
			(makeLine tree count str 1) ++ 
			"\n" ++ 
			(makeLinkLine tree count strLink 1) ++ 
			"\n" ++
			printTree tree (count + 1)

visualize :: Tree Char -> IO()
visualize tree = putStr (printTree tree 0)


-- test

t0 = Node {root = 'g', leftTree = Node {root = 'c', leftTree = Node {root = 'a', leftTree = Node {root = '0', leftTree = Empty, rightTree = Empty}, rightTree = Node {root = 'b', leftTree = Empty, rightTree = Empty}}, rightTree = Node {root = 'e', leftTree = Node {root = 'd', leftTree = Empty, rightTree = Empty}, rightTree = Node {root = 'f', leftTree = Empty, rightTree = Empty}}}, rightTree = Node {root = 'k', leftTree = Node {root = 'i', leftTree = Node {root = 'h', leftTree = Empty, rightTree = Empty}, rightTree = Node {root = 'j', leftTree = Empty, rightTree = Empty}}, rightTree = Node {root = 'm', leftTree = Node {root = 'l', leftTree = Empty, rightTree = Empty}, rightTree = Node {root = 'n', leftTree = Empty, rightTree = Empty}}}}
t1 = makeTree 'f' Empty (makeTree 'b' (makeLeaf 'c') (makeLeaf 'd'))
t2 = Empty
t3 = makeTree 'a' (makeTree 'c' (makeTree 'g' (makeLeaf 'j') (makeTree 'h' (makeLeaf 'i') Empty)) (makeTree 'd' Empty (makeLeaf 'e'))) (makeLeaf 'f')
t4 = makeTree 'a' (makeTree 'a' Empty (makeLeaf 'a')) (makeTree 'a' (makeLeaf 'a') Empty)

