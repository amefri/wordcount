import Test.HUnit
import Data.Char (toLower, isAlpha, isSpace)
import Data.List (sort)


-- Clean a string: Convert to lowercase and remove non-alphabetic characters
cleanString :: String -> String
cleanString = map toLower . filter (\c -> isAlpha c || isSpace c)

-- Tokenize text into words
tokenize :: String -> [String]
tokenize = words . cleanString

-- Red-Black Tree Definition
data Color = Red | Black deriving (Show, Eq)

data RBTree a
    = Empty
    | Node Color (RBTree a) a (RBTree a)
    deriving (Show)

-- Insert a word into the Red-Black Tree
insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = makeBlack (ins tree)
  where
    ins Empty = Node Red Empty x Empty
    ins (Node color left val right)
        | x < val = balance color (ins left) val right
        | x > val = balance color left val (ins right)
        | otherwise = Node color left val right -- Ignore duplicates

    makeBlack (Node _ left val right) = Node Black left val right
    makeBlack Empty = Empty

-- Balance the Red-Black Tree
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance color left val right = Node color left val right

-- In-order traversal of the Red-Black Tree
traverseInOrder :: RBTree a -> [a]
traverseInOrder Empty = []
traverseInOrder (Node _ left val right) =
    traverseInOrder left ++ [val] ++ traverseInOrder right

testCleanString :: Test
testCleanString = TestList
    [ "Remove special characters" ~: cleanString "Hello, World!" ~?= "hello world"
    , "Lowercase conversion" ~: cleanString "HELLO" ~?= "hello"
    , "Keep spaces" ~: cleanString "Hello World" ~?= "hello world"
    ]

-- Test für tokenize
testTokenize :: Test
testTokenize = TestList
    [ "Simple sentence" ~: tokenize "hello world" ~?= ["hello", "world"]
    , "With special characters" ~: tokenize "hello, world!" ~?= ["hello", "world"]
    , "Multiple spaces" ~: tokenize "hello    world" ~?= ["hello", "world"]
    ]

-- Test für insert und traverseInOrder
testTreeOperations :: Test
testTreeOperations = TestList
    [ "Insert and traverse single element" ~: traverseInOrder (insert "word" Empty) ~?= ["word"]
    , "Insert multiple elements" ~:
        let tree = foldr insert Empty ["banana", "apple", "cherry"]
        in traverseInOrder tree ~?= sort ["banana", "apple", "cherry"]
    , "Duplicate elements" ~: -- Doppelte Werte ignorieren
        let tree = foldr insert Empty ["apple", "apple", "banana"]
        in traverseInOrder tree ~?= ["apple", "banana"] -- Erwartete Ausgabe ohne Duplikate
    ]

-- Test für die gesamte Pipeline
testPipeline :: Test
testPipeline = TestCase $ do
    let content = "Hello, world! This is a test."
    let expectedWords = ["a", "hello", "is", "test", "this", "world"]
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sortedWords = traverseInOrder tree
    assertEqual "Pipeline: tokenized words" (sort tokenized) sortedWords
    assertEqual "Pipeline: sorted words" expectedWords sortedWords


-- Eq-Instanz für RBTree
instance Eq a => Eq (RBTree a) where
    Empty == Empty = True
    Node c1 l1 v1 r1 == Node c2 l2 v2 r2 = c1 == c2 && l1 == l2 && v1 == v2 && r1 == r2
    _ == _ = False



-- Tests Tree
testBalanceCase1 :: Test
testBalanceCase1 = TestCase $ do
    let unbalancedTree :: RBTree Int
        unbalancedTree = Node Black (Node Red (Node Red Empty 1 Empty) 2 Empty) 3 Empty

    let expectedTree :: RBTree Int
        expectedTree = Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)

    let resultTree = balance Black (Node Red (Node Red Empty 1 Empty) 2 Empty) 3 Empty
    assertEqual "Balance Case 1: Leftchild of Leftchild is red" expectedTree resultTree

testBalanceCase2 :: Test
testBalanceCase2 = TestCase $ do
    
    let unbalancedTree :: RBTree Int
        unbalancedTree = Node Black (Node Red Empty 1 (Node Red Empty 2 Empty)) 3 Empty

    let expectedTree :: RBTree Int
        expectedTree = Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)

   
    let balancedTree = balance Black (Node Red Empty 1 (Node Red Empty 2 Empty)) 3 Empty
    assertEqual "Balance Case 2: Rightchild of Leftchild is red" expectedTree balancedTree






-- Hauptfunktion für die Tests
main :: IO ()
main = do
    runTestTTAndExit $ TestList
        [ testCleanString
        , testTokenize
        , testTreeOperations
        , testPipeline
        , testBalanceCase1
        , testBalanceCase2
        
        ]


-- 	testCleanString - 	Entfernt Sonderzeichen aus dem String.
--	testCleanString - 	Wandelt Großbuchstaben in Kleinbuchstaben um.
--	testCleanString - 	Behält Leerzeichen bei.
--	testTokenize - 	Zerlegt einen einfachen Satz in Wörter.
--	testTokenize - 	Entfernt Sonderzeichen und zerlegt den Text in Wörter.
--	testTokenize - 	Ignoriert mehrere aufeinanderfolgende Leerzeichen.
--	testTreeOperations - Fügt ein einzelnes Element in den Baum ein und traversiert ihn.
--	testTreeOperations - Fügt mehrere Elemente ein und überprüft die In-Order-Traversierung.
--	testTreeOperations - Ignoriert doppelte Elemente im Baum.
--	testPipeline	Überprüft die gesamte Datenverarbeitungspipeline.
-- testBalanceCase1 - LLinks von Links ist rot
--  testBalanceCase2 -  Rightchild des Leftchild ist rot
        