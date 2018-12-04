data Item= I String deriving(Show,Eq)
data User= U String deriving(Show,Eq)
data Fractional a => Rating a= R a | NoRating deriving(Show,Eq)
mem :: Eq a => a -> [a] -> Bool
mem _ []=False
mem h (x:t)=if(x==h) then True else (mem h t)

dis :: Eq a => [a] -> [a]
dis []=[]
dis (h:t)=if(mem h t) then dis(t) else h:(dis t)

fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems a=dis (helperFRTI a)
helperFRTI []=[]
helperFRTI ((_,b,_):t)=(b:(helperFRTI t))

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers a =dis(helperRTU a)
helperRTU[]=[]
helperRTU ((a,_,_):t)=(a:(helperRTU t))

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ []=False
hasRating u i ((a,b,_):t)=if(u==a && i==b) then True else (hasRating u i t)

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating u i l=if( hasRating u i l ) then (helperGR u i l) else error"No given rating"
helperGR u i ((a,b,c):t)=if(a==u && i==b) then c else (helperGR u i t)

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser _ [] _=[]
formMatrixUser u (i:is) l=if(hasRating u i l) then (R (getRating u i l) :(formMatrixUser u is l)) 
						  else  (NoRating :(formMatrixUser u is l))

formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] i l=[]
formMatrix (u:us) i l=((formMatrixUser u i l):(formMatrix us i l))

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem _ []=0
numberRatingsGivenItem a (lr:lrs)=if((helperNRGI a 0 lr)/=NoRating) then 1+(numberRatingsGivenItem a lrs) else (numberRatingsGivenItem a lrs)
helperNRGI a i (r:t)=if(a==i) then r else (helperNRGI a (i+1) t)

differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _=0.0
differeneRatings _ NoRating=0.0
differeneRatings (R x) (R y)=x-y

matrixPairs :: Num a => a -> [(a,a)]
matrixPairs x=helperMP 0 0 x
helperMP i j x=if(i==x) then [] else 
								if(j==x) then helperMP (i+1) 0 x else (i,j):helperMP i (j+1) x
								
--The function dMatrix :: Fractional a => [[Rating a]] -> [a] takes the ratings’ matrix and returns
--a matrix for the items ratings differences. The value at row i and column j represents the sum of
--the differences between the ratings given to item i and item j by the same user.
--Examples:
--items=		0,		1
--user1=	0 [[NoRating,R 5.0],
--user2=	  [R 5.0   ,R 4.0],
--user3=	  [R 3.0   ,R 1.0],
--	     0    1
--	 0 [0.0, 3.0,
--	 1 -3.0,0.0]
--		0	1
--	0	[2, 2,
--	1	2,	3]
--		0	  1
--	0	[0.0, 1.5,
--		-1.5, 0.0]
--		[(0,0),(0,1),(1,0),(1,1)]

dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix (r:t)= helperDM (matrixPairs (length r)) (r:t)
helperDM [] _=[]
helperDM (p:t) l=(helperDM2 p l):(helperDM t l)
helperDM2 _ []=0
helperDM2 (x,y) (r:t)=(differeneRatings (helperDM3 x r) (helperDM3 y r))+(helperDM2 (x,y) t)
helperDM3 i (r:t)=if(i==0) then r else helperDM3 (i-1) t

freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (r:t)= helperFM (matrixPairs (length r)) (r:t)
helperFM [] _=[]
helperFM (p:t) l=(helperFM2 p l):(helperFM t l)
helperFM2 _ []=0
helperFM2 (x,y) (r:t)=if( (helperFM3 x r)/=NoRating && (helperFM3 y r)/=NoRating) then 1+(helperFM2 (x,y) t)
						else (helperFM2 (x,y) t)
helperFM3 x (r:t)=if(x==0) then r else helperFM3 (x-1) t

diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix l=helperDFM (dMatrix l) (freqMatrix l) 
helperDFM [] []=[]
helperDFM (r:l) (n:ns)= (r/n):(helperDFM l ns)

plus _ NoRating=0
plus y (R x)=x+y

predict :: (Fractional a, Num b) => [(User,Item,a)] -> b -> Int -> a
predict l iu ir =if( (chechkrating2 (chechkrating (predicthelp1 l) iu) ir)==NoRating) 
											then predicthelp (predicthelp1 l) iu ir 
				 else getNumber((chechkrating2 (chechkrating (predicthelp1 l) iu) ir))

-- get the matrix of ratings				 
predicthelp1 l= formMatrix (dis(getuser l)) (dis(getitems l)) l

getuser []= []
getuser ((U s,_,_):rs)=(U s):(getuser rs)
getitems []=[]
getitems ((_,I s,_):rs)=(I s):(getitems rs)
getNumber (R x)=x

predicthelp (r:rs) iu ir=(helperP2 (helperP (diffFreqMatrix (r:rs)) (matrixPairs (length r)) ir) (helperP1 (r:rs) iu)) /(lengthYNY r -1)
chechkrating2 (x:xs) ir=if(ir==0) then x else chechkrating2 xs (ir-1)
chechkrating (r:rs) iu=if(iu==0) then r else chechkrating rs (iu-1)

-- return all the average ratings with respect the item i want [-3.0,0.0,1.0]
helperP [] [] _=[]
helperP (d:ds) ((x,y):ps) ir=if(ir==x) then d:(helperP ds ps ir) else (helperP ds ps ir)

--helperP1 return the list of items of the User i [R 4.0,NoRating,R 3.0]
helperP1 (r:rs) i=if(i==0) then r else (helperP1 rs (i-1))

--helperP2 [-3.0,0.0,1.0] [R 4.0,NoRating,R 3.0]
helperP2 [] []=0
helperP2 (d:ds) (ur:urs)=(plus d ur)+(helperP2 ds urs)

lengthYNY []=0
lengthYNY (x:xs)=1+lengthYNY xs

u =[(U "Caroll",I "Lonovo", 4.0),(U "Caroll",I "Sofa",3.0),(U "Nada",I "Honda",4.0),(U "Nada",I "Sofa", 2.0),(U "Ahmed", I "Lonovo", 3.0),(U "Ahmed", I "Honda", 1.0),(U "Ahmed", I "Sofa",1.0)]
--		0	1			2
-- [[R 4.0,NoRating,R 3.0],
--	[NoRating,R 4.0,R 2.0],
--	[R 3.0,R 1.0,	R 1.0]]

--item 1: NoRating - 4.0=-2.0 then NoRating=2.0
--item 1: NoRating - 3.0= 1.0 then NoRating=4.0
--item 1: NoRating=6/2=3
 
--	0		1		2
--0[0.0,	2.0,	3.0,
--1 -2.0,	0.0,	2.0,
--2 -3.0,	-2.0,	0.0]

--	  0		1	2	
-- 0 [2,	1,	2,
-- 1  1,	2,	2,
-- 2  2,	2,	3]

--	 0		1		2
--0 [0.0,	2.0,	1.5,
--1 -2.0,	0.0,	1.0,
--2 -1.5,	-1.0,	0.0]












