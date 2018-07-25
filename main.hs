import FurnitureResources
import Data.List

-- *Training Phase START*

-- statsList & helpers START

-- statsList
statsList :: [([Char],[[([Char],[Char],Int)]])]
statsList = statsList1 (statsListUnsorted)

-- statsList1, statsList helper
statsList1 [] = []
statsList1 pairsList =
	do
		let ((objectX, [rightList,belowList]):pairs) = pairsList
		let newRightList = superCoolAhmedSort rightList
		let newBelowList = superCoolAhmedSort belowList
		(objectX,[newRightList,newBelowList]) : statsList1 pairs

-- superCoolAhmedSort, statsList1 helper
superCoolAhmedSort [] = []
superCoolAhmedSort list =  (maxL list) : superCoolAhmedSort(delete1 (maxL list) list)		

-- delete1, superCoolAhmedSort helper
delete1  (x,y,z) ((x1,y1,z1):triples) = if (x==x1) then triples
	else
		(x1,y1,z1) : delete1 (x,y,z) triples

-- maxL, superCoolAhmedSort helper
maxL ((x,y,z):triples) = maxLHelper ((x,y,z):triples) (x,y,z)

-- maxLHelper, maxL helper
maxLHelper [] max1 = max1
maxLHelper ((x,y,z):triples) (x1,y1,z1) = if (z>=z1) then maxLHelper triples (x,y,z) 
	else 
		maxLHelper triples (x1,y1,z1) 

-- statsListUnsorted, statsList helper
statsListUnsorted = statsList1stHelper training [("couch",[[],[]]), ("table",[[],[]]), ("lamp",[[],[]]), ("tv",[[],[]]), ("e",[[],[]])]

-- statsList1stHelper, statsListUnsorted helper
statsList1stHelper [] statsList = statsList
statsList1stHelper rooms statsList = 
	do
		let (x:xs) = rooms
		let newStatsList = statsList2ndHelper x statsList
		statsList1stHelper xs newStatsList

-- statsList2ndHelper, statsList1stHelper helper
statsList2ndHelper room statsList = generate room statsList
-- statsList & helpers END


-- generate & helpers START

-- generate
generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]
generate room statsList = 
	do 
		let statsListTemp = generateHelperRight room statsList
		generateHelperBelow (transpose room) statsListTemp

-- generateHelperRight, generate's helper
generateHelperRight [[]] statsList = statsList
generateHelperRight ((x:[]):[]) statsList = statsList
generateHelperRight ((x:[]):(y:ys)) statsList = generateHelperRight (y:ys) statsList
generateHelperRight ([]:(y:ys)) statsList = generateHelperRight (y:ys) statsList
generateHelperRight ((x:x1:xs):ys)  statsList = 
	do 
		let statsListTemp = findFurnitureUpdate x x1 "right" statsList
		generateHelperRight ((x1:xs):ys) statsListTemp

-- generateHelperBelow, generate's helper
generateHelperBelow [[]] statsList = statsList
generateHelperBelow ((x:[]):[]) statsList = statsList
generateHelperBelow ((x:[]):(y:ys)) statsList = generateHelperBelow (y:ys) statsList
generateHelperBelow ([]:(y:ys)) statsList = generateHelperBelow (y:ys) statsList
generateHelperBelow ((x:x1:xs):ys)  statsList = 
	do 
		let statsListTemp = findFurnitureUpdate x x1 "below" statsList
		generateHelperBelow ((x1:xs):ys) statsListTemp
-- generate & helpers END


-- findFurnitureUpdate & helpers START

-- findFurnitureUpdate
findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]
findFurnitureUpdate a b c ((objectX, [rightList,belowList]):pairs)
	| c == "right" = findFurnitureUpdateRightHelper a b c ((objectX, [rightList,belowList]):pairs)
	| c == "below" = findFurnitureUpdateBelowHelper a b c ((objectX, [rightList,belowList]):pairs)
	| otherwise = error("Yo!, we agreed to use 2 directions only (right/below)!!, stick to the rules !!!")


-- findFurnitureUpdateRightHelper, findFurnitureUpdate helper
findFurnitureUpdateRightHelper a b "right" [] = [ (a, [[(b,"right",1)],[]]) ]

findFurnitureUpdateRightHelper a b "right" ((objectX, [[],belowList]) : pairs) = if (a==objectX) then ((objectX, [[(b,"right",1)],belowList]): pairs)
	else [(objectX, [[],belowList])] ++ findFurnitureUpdateRightHelper a b "right" pairs

findFurnitureUpdateRightHelper a b "right" ((objectX, [((x,y,z):rightList),belowList]):pairs)
	| a == objectX = ((objectX, [(addOrIncrement b "right" ((x,y,z):rightList)),belowList]): pairs) 
	| a /= objectX = [(objectX, [((x,y,z):rightList),belowList])]  ++ findFurnitureUpdateRightHelper a b "right" pairs


-- findFurnitureUpdateBelowHelper, findFurnitureUpdate helper
findFurnitureUpdateBelowHelper a b "below" [] = [ (a, [[],[(b,"below",1)]]) ]	

findFurnitureUpdateBelowHelper a b "below" ((objectX, [rightList,[]]) : pairs) = if (a==objectX) then ((objectX, [rightList,[(b,"below",1)]]): pairs)
	else [(objectX, [rightList,[]])] ++ findFurnitureUpdateBelowHelper a b "below" pairs

findFurnitureUpdateBelowHelper a b "below" ((objectX, [rightList,((x,y,z):belowList)]):pairs)
	| a == objectX = ((objectX, [rightList,addOrIncrement b "below" ((x,y,z):belowList)]): pairs) 
	| a /= objectX = [(objectX, [rightList,((x,y,z):belowList)])]  ++ findFurnitureUpdateBelowHelper a b "below" pairs


-- addOrIncrement, findFurnitureUpdateRightHelper & findFurnitureUpdateBelowHelper helper
addOrIncrement  b c [] = [(b,c, 1)]
addOrIncrement  b c ((x,y,z):list) = if (b==x) then ((x,y,z+1):list) 
	else [(x,y,z)] ++ addOrIncrement  b c list
-- findFurnitureUpdate END

-- *Training Phase END*




-- *Generation Phase START*

-- furnishRoom & helpers START

-- furnishRoom
furnishRoom :: Int -> [Char] -> [[[Char]]]
furnishRoom n objectX = 
	do
		let room = reverse (createRoom [[objectX]] (n-1))
		furnishRoomHelper n 0 1 room

createRoom room 0 = room
createRoom room n = []:createRoom room (n-1)

-- furnishRoomHelper,  furnishRoom helper
furnishRoomHelper n row col room = 
	if col < n then
		do
			let list1 = getFromLeft row (col-1) room
			let list2 = getFromUp (row-1) col room
			let entry = getPossibleNeighbour list1 list2
			let newRow = (room !! row) ++ [entry]
			let newRoom = buildRoom 0 n row newRow room
			furnishRoomHelper n row (col+1) newRoom
	else
		if(col==n && row==n) then room
			else
				furnishRoomHelper n (row+1) 0 room


-- getFromLeft,  furnishRoomHelper helper
getFromLeft row col room = 
	if (row == (-1) || col == (-1)) then []
		else
			( (getFurnStat ((room !! row) !! col)) ) !! 0


-- getFromUp,  furnishRoomHelper helper
getFromUp row col room = 
	if (row == (-1) || col == (-1)) then []
		else
			( (getFurnStat ((room !! row) !! col)) ) !! 1		

-- buildRoom,  furnishRoomHelper helper
buildRoom i n rowIndex newRow room = 
	if (i==n) then []
		else
			if (i==rowIndex) then newRow : buildRoom (i+1) n rowIndex newRow room
				else
					(room !! i) : buildRoom (i+1) n rowIndex newRow room
-- furnishRoom & helpers END


--getFurnStat & helpers START

-- getFurnStat
getFurnStat :: [Char] -> [[([Char],[Char],Int)]]
getFurnStat objectX = getFurnStatHelper objectX  (statsList)

-- getFurnStatHelper, getFurnStat helper
getFurnStatHelper objectX [] = error ("Invalid furniture piece")
getFurnStatHelper objectX  ((objectY, [rightList,belowList]):pairs) = 
	if objectX == objectY then [rightList,belowList]
		else 
			getFurnStatHelper objectX pairs
--getFurnStat & helpers END


-- getPossibleNeighbour & helpers START

-- getPossibleNeighbour 
getPossibleNeighbour :: [([Char],[Char],Int)] -> [([Char],[Char],Int)] -> [Char]
getPossibleNeighbour rightList belowList = 
	do
		let list = getPossibleNeighbourHelper rightList belowList
		list !! randomZeroToX ((length list)-1)

-- getPossibleNeighbourHelper, getPossibleNeighbour helper
getPossibleNeighbourHelper [] [] = []

getPossibleNeighbourHelper ((x,y,z):rightList) [] = 
	(expand x z) ++ getPossibleNeighbourHelper rightList []

getPossibleNeighbourHelper [] ((x1,y1,z1):belowList) = 
	(expand x1 z1) ++ getPossibleNeighbourHelper [] belowList	

getPossibleNeighbourHelper ((x,y,z):rightList) ((x1,y1,z1):belowList) = 
	(expand x z) ++ (expand x1 z1) ++ getPossibleNeighbourHelper rightList belowList

-- expand, getPossibleNeighbourHelper helper
expand item 0 = []
expand item n = item : expand item (n-1)
--getPossibleNeighbour & helpers END

-- *Generation Phase END*