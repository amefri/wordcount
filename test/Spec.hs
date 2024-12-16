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

-- Tests for string cleaning
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



-- Test for Red-Black Tree balancing
testBalanceCase1 :: Test
testBalanceCase1 = TestCase $ do
    let expectedTree :: RBTree Int
        expectedTree = Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)

    let resultTree = balance Black (Node Red (Node Red Empty 1 Empty) 2 Empty) 3 Empty
    assertEqual "Balance Case 1: Leftchild of Leftchild is red" expectedTree resultTree

testBalanceCase2 :: Test
testBalanceCase2 = TestCase $ do
    let expectedTree :: RBTree Int
        expectedTree = Node Red (Node Black Empty 1 Empty) 2 (Node Black Empty 3 Empty)

    let balancedTree = balance Black (Node Red Empty 1 (Node Red Empty 2 Empty)) 3 Empty
    assertEqual "Balance Case 2: Rightchild of Leftchild is red" expectedTree balancedTree


-- Edge case: Empty input
testEmptyInput :: Test
testEmptyInput = TestCase $ do
    let content = ""
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sortedWords = traverseInOrder tree
    assertEqual "Empty input should result in empty output" [] sortedWords

-- Edge case: Punctuation-only input
testPunctuationOnly :: Test
testPunctuationOnly = TestCase $ do
    let content = "!!! ??? ,,, ..."
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sortedWords = traverseInOrder tree
    assertEqual "Punctuation-only input should result in empty output" [] sortedWords

-- Edge case: Large dataset
testLargeDataset :: Test
testLargeDataset = TestCase $ do
    let content = unwords (replicate 100000 "test")
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sortedWords = traverseInOrder tree
    assertEqual "Large dataset should handle high volume of data" ["test"] sortedWords


testDeepTree :: Test
testDeepTree = TestCase $ do
    let elements :: [Int]
        elements = [1..100000] -- Explizite Typannotation auf Int
    let tree = foldr insert Empty elements
    let sorted = traverseInOrder tree
    assertEqual "Deep tree with large unique input" elements sorted


testCaseInsensitive :: Test
testCaseInsensitive = TestCase $ do
    let content = "Hello HELLO HeLLo"
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sorted = traverseInOrder tree
    assertEqual "Case insensitivity check" ["hello"] sorted


testNumbersAndSpecialChars :: Test
testNumbersAndSpecialChars = TestCase $ do
    let content = "123 456 !@# hello"
    let tokenized = tokenize content
    let tree = foldr insert Empty tokenized
    let sorted = traverseInOrder tree
    assertEqual "Numbers and special characters ignored" ["hello"] sorted

testDescendingOrder :: Test
testDescendingOrder = TestCase $ do
    let elements :: [Int]
        elements = reverse [1..100] -- Explizite Typannotation auf Int
    let tree = foldr insert Empty elements
    let sorted = traverseInOrder tree
    assertEqual "Descending order input" ([1..100] :: [Int]) sorted


testManyDuplicates :: Test
testManyDuplicates = TestCase $ do
    let elements = replicate 10000 "hello"
    let tree = foldr insert Empty elements
    let sorted = traverseInOrder tree
    assertEqual "Large number of duplicate entries" ["hello"] sorted


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
        , testEmptyInput
        , testPunctuationOnly
        , testLargeDataset
        , testDeepTree
        , testCaseInsensitive
        , testNumbersAndSpecialChars
        , testDescendingOrder
        , testManyDuplicates
        ]


-- testCleanString - 	Entfernt Sonderzeichen aus dem String.
-- testCleanString - 	Wandelt Großbuchstaben in Kleinbuchstaben um.
-- testCleanString - 	Behält Leerzeichen bei.
-- testTokenize - 	Zerlegt einen einfachen Satz in Wörter.
-- testTokenize - 	Entfernt Sonderzeichen und zerlegt den Text in Wörter.
-- testTokenize - 	Ignoriert mehrere aufeinanderfolgende Leerzeichen.
-- testTreeOperations - Fügt ein einzelnes Element in den Baum ein und traversiert ihn.
-- testTreeOperations - Fügt mehrere Elemente ein und überprüft die In-Order-Traversierung.
-- testTreeOperations - Ignoriert doppelte Elemente im Baum.
-- testPipeline	Überprüft die gesamte Datenverarbeitungspipeline.
-- testBalanceCase1 - Links von Links ist rot
-- testBalanceCase2 -  Rightchild des Leftchild ist rot
-- testEmptyInput - Testet, ob der Algorithmus leere Eingaben korrekt verarbeitet und eine leere Ausgabe erzeugt.
-- testPunctuationOnly - Verarbeitet Eingaben, die nur aus Satzzeichen bestehen, und prüft, ob keine Ausgabe generiert wird.
-- testLargeDataset - Simuliert die Verarbeitung eines sehr großen Datensatzes (100.000 Wörter), um die Effizienz und Korrektheit zu gewährleisten.
-- testBalanceCases - Testet verschiedene Balancierungsfälle, insbesondere wenn das linke oder rechte Kind eines roten Knotens selbst rot ist.
-- testLargeDataset - Simuliert Verarbeitung eines großen Datensatzes (100.000 Wörter), um die Effizienz und Korrektheit zu gewährleisten.
-- testDeepTree - Fügt viele eindeutige Zahlen (1 bis 100.000) ein und prüft, ob der Baum korrekt balanciert ist und die Zahlen aufsteigend zurückgibt.
-- testCaseInsensitive - Fügt Wörter mit gemischter Groß-/Kleinschreibung ein und prüft, ob sie fallunempfindlich korrekt zusammengefasst werden.
-- testNumbersAndSpecialChars - Verarbeitet Zahlen und Sonderzeichen im Eingabetext und stellt sicher, dass sie ignoriert werden.
-- testDescendingOrder - Fügt Zahlen in absteigender Reihenfolge ein und prüft, ob die In-Order-Traversierung die Zahlen aufsteigend zurückgibt.
-- testManyDuplicates - Fügt viele doppelte Wörter (10.000 Mal "hello") in den Baum ein und prüft, ob Duplikate korrekt ignoriert werden.        