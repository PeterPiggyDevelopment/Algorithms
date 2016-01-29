--import Debug.Trace

type Graph = [Way]
type Way = (Node, Node, Int)
type Node = Int

main :: IO ()
main =  print $ findShortest 1 7 graph
    where graph = [(1,2,2)
                  ,(2,3,1)
                  ,(3,4,10)
                  ,(4,11,5)
                  ,(4,6,1)
                  ,(1,5,6)
                  ,(5,6,9)
                  ,(6,7,8)
                  ,(7,11,3)
                  ,(1,8,2)
                  ,(8,9,30)
                  ,(9,10,5)
                  ,(3,10,20)
                  ,(10,11,1)
                  ]

findShortest :: Node -> Node -> Graph -> Graph
findShortest bnode enode gr = 
           if enode `elem` map second (findWaysWith bnode gr)
              then [(bnode, enode, findWeightBetween bnode enode gr)]
              else foldl minList [] (foldl addc [] (getMinWays bnode enode gr))
                            where 
                              addc grls []= grls
                              addc grls gr1= ((bnode, first (head gr1),
                                    findWeightBetween bnode (first (head gr1)) gr):gr1):grls
                              getMinWays b e gr1 = map (find . second) (findWaysWith b gr1)
                                           where find n = findShortest n e gr1

minList :: Graph -> Graph -> Graph
minList [] g1 = g1
minList g1 g2 = if summ g1 > summ g2 then g2 else g1
                     
summ :: Graph -> Int
summ = foldr (\(_, _, w) su ->  su + w) 0

findWaysWith :: Node -> Graph -> [Way]
findWaysWith n = foldl (collectNodeWith n) []

findWeightBetween :: Node -> Node -> Graph -> Int
findWeightBetween b e gr = thd $ foldl findSecond (0,0,0) (findWaysWith b gr)
            where findSecond xs (a,beg,w) = if e == beg
                          then (a,beg,w)
                          else xs

first :: (a, b, c) -> a
first (a,_,_) = a

second :: (a, b, c) -> b
second (_,b,_) = b

thd :: (a, b, c) -> c
thd (_,_,c) = c

collectNodeWith :: Node -> [Way] -> Way -> [Way]
collectNodeWith n [] (a, b, w) | a == n = [(a,b,w)]
                            | otherwise = []
collectNodeWith n xs (a, b, w) | a == n = (a,b,w):xs
                            | otherwise = xs
