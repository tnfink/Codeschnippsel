module AlgebraischeDatentypen
( binSearch
, BinTreeT
, createBinTree
, adSpecifications
)
where 

import Test.Hspec
import Test.QuickCheck


data BinTreeT a = 
   Tree (BinTreeT a) a (BinTreeT a) | 
   Empty
  deriving Show                

-- |binSearch searches for an element in the tree
---------------------------------------------------
binSearch :: (Ord a) => a -> BinTreeT a -> Bool
binSearch _ Empty = False
binSearch search (Tree left value right) 
  | search < value  = binSearch search left
  | search > value  = binSearch search right
  | search == value = True
  | otherwise = error "there seems to be an implementation error"
  
binSearchSpec :: Spec
binSearchSpec =
  describe "binSearch" $ do
    let sub1 = Tree Empty "a" Empty
        sub2 = Tree (Tree Empty "c" Empty) "d" (Tree Empty "e" Empty)
        tree = Tree  sub1 "b" sub2
    it "returns false for an empty tree" $
      binSearch "a" Empty `shouldBe` False
    it "returns false if the value is not contained in the sorted tree" $
      binSearch "x" tree `shouldBe` False 
    it "returns true if the value is contained in the sorted tree" $
      binSearch "e" tree `shouldBe` True

-- |binInsert inserts a value to a tree, if the tree already contains the value
-- |  it will return the same tree. This means the tree acts as a set.
------------------------------------------------------

binInsert :: (Ord a) => a -> BinTreeT a -> BinTreeT a
binInsert newValue Empty = Tree Empty newValue Empty
binInsert newValue tree@ (Tree left val right)
  | newValue == val = tree
  | newValue < val = Tree (binInsert newValue left) val right
  | newValue > val = Tree left val (binInsert newValue right)
  | otherwise = error "This should not happen."

binInsertSpec :: Spec
binInsertSpec =
  describe "binInsertSpec" $ do
    let testTree :: BinTreeT String -> String -> Bool
        testTree tree ele = binSearch ele insertedTree
          where insertedTree = binInsert ele tree  
    it "can add an element to an empty tree" $
      property $ testTree Empty
    it "can add an a lement to a tree containing 1 String" $ do
      let tree = Tree Empty "string" Empty
      property $ testTree tree
    it "can add an a element to a tree containing 3 Strings" $ do
      let sub1 = Tree Empty "aaaaaa" Empty
          sub2 = Tree Empty "ZZZZZZ" Empty
          tree = Tree sub1 "nnnnnnn" sub2 
      property $ testTree tree

-- |creates a tree from a list
------------------------------------------------------

createBinTree = error "tbd"



------------------------------------------------------
adSpecifications :: [Spec]
adSpecifications = [binSearchSpec,binInsertSpec]



