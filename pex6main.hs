-- pex6.hs 
-- unKnot Haskell

-- name: Samuel Lavoie

{- DOCUMENTATION:
Used Gavin Smith's test cases for my code. 
Used https://stackoverflow.com/questions/65653918/how-to-move-all-occurrences-of-the-first-element-of-the-list-to-the-end-of-the-l to help with the wrapThisTripCode function.
Used https://stackoverflow.com/questions/53914134/haskell-function-that-goes-through-each-pair-in-list-and-returns-a-list-with-siz to help with the typeOneMoveExistsInThisTripCode function.
Used https://www.some.ox.ac.uk/wp-content/uploads/2020/08/HaskellTutorial.pdf to help with haskell basics/syntax.
Used https://en.wikibooks.org/wiki/Haskell/Pattern_matching to help with pattern matching.
Used https://www.youtube.com/watch?v=RJjETqGwk5M to help with pattern matching.
Used https://stackoverflow.com/questions/2225774/haskell-pattern-matching-what-is-it to help with pattern matching.
-}
unKnot :: [(Char, Char)] -> String 
unKnot tripCode
   | null tripCode = "not a knot"
   | typeOneMoveExistsInThisTripCode tripCode = unKnot (makeTypeOneMoveInThisTripCode tripCode)
   | typeOneMoveExistsInThisTripCode (wrapThisTripCode tripCode) = unKnot (makeTypeOneMoveInThisTripCode (wrapThisTripCode tripCode))
   | typeTwoMoveExistsInThisTripCode tripCode = unKnot (makeTypeTwoMoveInThisTripCode tripCode)
   | typeTwoMoveExistsInThisTripCode (wrapThisTripCode tripCode) = unKnot (makeTypeTwoMoveInThisTripCode (wrapThisTripCode tripCode))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

wrapThisTripCode :: [(Char, Char)] -> [(Char, Char)] -- move first element to end
wrapThisTripCode [] = []
wrapThisTripCode (x:xs) = xs ++ [x]

typeOneMoveExistsInThisTripCode :: [(Char, Char)] -> Bool -- checks for type 1 knot
typeOneMoveExistsInThisTripCode [] = False
typeOneMoveExistsInThisTripCode [_] = False
typeOneMoveExistsInThisTripCode ((c1, t1):(c2, t2):xs) =
   if c1 == c2
      then True
      else typeOneMoveExistsInThisTripCode ((c2, t2):xs)

makeTypeOneMoveInThisTripCode :: [(Char, Char)] -> [(Char, Char)] --unties type 1
makeTypeOneMoveInThisTripCode [] = []
makeTypeOneMoveInThisTripCode [x] = [x]
makeTypeOneMoveInThisTripCode ((c1, t1):(c2, t2):xs) =
   if c1 == c2
      then xs
      else (c1, t1) : makeTypeOneMoveInThisTripCode ((c2, t2):xs)

hasSecond :: Char -> Char -> Char -> [(Char, Char)] -> Bool -- checks for adjacent paris with same crossing/opposite type
hasSecond _ _ _ [] = False
hasSecond _ _ _ [_] = False
hasSecond c1 c2 t1 ((d1, u1):(d2, u2):xs) =
   if u1 == u2
      then if u1 /= t1
         then if (c1 == d1 && c2 == d2) || (c1 == d2 && c2 == d1)
            then True -- if same names and opposite types
            else hasSecond c1 c2 t1 ((d2, u2):xs)
         else hasSecond c1 c2 t1 ((d2, u2):xs)
      else hasSecond c1 c2 t1 ((d2, u2):xs)

removeSecond :: Char -> Char -> Char -> [(Char, Char)] -> [(Char, Char)] -- removes first valid pair for type 2
removeSecond _ _ _ [] = []
removeSecond _ _ _ [x] = [x]
removeSecond c1 c2 t1 ((d1, u1):(d2, u2):xs) =
   if u1 == u2
      then if u1 /= t1 -- if same crossing/opposite type
         then if (c1 == d1 && c2 == d2) || (c1 == d2 && c2 == d1) -- if same names and opposite types
            then xs
            else (d1, u1) : removeSecond c1 c2 t1 ((d2, u2):xs)
         else (d1, u1) : removeSecond c1 c2 t1 ((d2, u2):xs)
      else (d1, u1) : removeSecond c1 c2 t1 ((d2, u2):xs)

typeTwoMoveExistsInThisTripCode :: [(Char, Char)] -> Bool -- checks for type 2
typeTwoMoveExistsInThisTripCode [] = False
typeTwoMoveExistsInThisTripCode [_] = False
typeTwoMoveExistsInThisTripCode ((c1, t1):(c2, t2):xs) =
   if t1 == t2
      then if hasSecond c1 c2 t1 xs
         then True -- if same type/second exists
         else typeTwoMoveExistsInThisTripCode ((c2, t2):xs)
      else typeTwoMoveExistsInThisTripCode ((c2, t2):xs)

makeTypeTwoMoveInThisTripCode :: [(Char, Char)] -> [(Char, Char)] -- unties type 2
makeTypeTwoMoveInThisTripCode [] = []
makeTypeTwoMoveInThisTripCode [x] = [x]
makeTypeTwoMoveInThisTripCode ((c1, t1):(c2, t2):xs) =
   if t1 == t2
      then if hasSecond c1 c2 t1 xs
         then removeSecond c1 c2 t1 xs
         else (c1, t1) : makeTypeTwoMoveInThisTripCode ((c2, t2):xs)
      else (c1, t1) : makeTypeTwoMoveInThisTripCode ((c2, t2):xs)


main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnot t02)

   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03)

   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04)

   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnot t05)

   let t06 = [('a','o'),('q','u'),('a','u')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t06)

   let t07 = [('a','o'),('a','u'),('q','u')]
   print("   test case t07 - tripcode: " )
   print(t07)
   print("   result:" ++ unKnot t07)

   let t08 = [('a','o'),('b','o'),('a','u'),('b','u'),('q','u')]
   print("   test case t08 - tripcode: " )
   print(t08)
   print("   result:" ++ unKnot t08)

   let t09 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u')] -- Test failed
   print("   test case t09 - tripcode: " )
   print(t09)
   print("   result:" ++ unKnot t09)

   let t10 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u'),('c','o'),('c','u')] -- Test failed
   print("   test case t10 - tripcode: " )
   print(t10)
   print("   result:" ++ unKnot t10)

   let t11 = [('a','u'),('b','o'),('a','o'),('q','u'),('b','u'),('c','o'),('c','u')]
   print("   test case t11 - tripcode: " )
   print(t11)
   print("   result:" ++ unKnot t11)

   let t12 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t12 - tripcode: " )
   print(t12)
   print("   result:" ++ unKnot t12)
