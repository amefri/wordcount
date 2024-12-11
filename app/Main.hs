import Data.Char (toLower, isAlpha, isSpace)
import Data.List (words)
import System.CPUTime

-- Zeitmessung für IO-Aktionen
measureTime :: IO a -> IO a
measureTime action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) -- Zeit in Sekunden
    putStrLn $ "Laufzeit: " ++ show diff ++ " Sekunden"
    return result

-- Zeitmessung für reine Funktionen
measurePureTime :: a -> IO a
measurePureTime result = do
    start <- getCPUTime
    let evaluated = result `seq` result -- Erzwingt Auswertung
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) -- Zeit in Sekunden
    putStrLn $ "Laufzeit: " ++ show diff ++ " Sekunden"
    return evaluated

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
insert :: Ord a => a -> RBTree a -> RBTree a -- nimmt baum gibt mit neuem element zurück
insert x tree = makeBlack (ins tree)
  where
    ins Empty = Node Red Empty x Empty -- wenn baum leer neuer baum knoten ist rot
    ins (Node color left val right) -- wenn nicht leer 
        | x < val = balance color (ins left) val right
        | x > val = balance color left val (ins right)
        | otherwise = Node color left val right -- dublikate werden ingnoriert

    makeBlack (Node _ left val right) = Node Black left val right
    makeBlack Empty = Empty

-- Balance the Red-Black Tree
-- Funktion: balance
-- Stellt sicher, dass der Red-Black-Baum nach dem Einfügen eines Knotens balanciert bleibt.
-- Parameter:
--  Color: Die Farbe des aktuellen Knotens.
-- RBTree a: RBTree
-- a: Der Wert des aktuellen Knotens.
-- Rückgabewert: Ein balancierter Red-Black-Baum.

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
-- Fall 1: Leftchild des Leftchild ist rot
-- Der linke Teilbaum hat ein rotes Child (y), dessen eigenes Child (x) ebenfalls rot ist.
-- Lösung: Rotationsumstrukturierung und Umfärbung der Knoten.
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
-- Fall 2: Rightchild des Leftchild ist rot
-- Der linke Teilbaum hat ein rotes Child (x), dessen rechtes Child (y) rot ist.
-- Lösung: Rotationsumstrukturierung und Umfärbung der Knoten.
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
-- Fall 3: Leftchild des Rightchild ist rot
-- Der rechte Teilbaum hat ein rotes Child (z), dessen linkes Child (y) rot ist.
-- Lösung: Rotationsumstrukturierung und Umfärbung der Knoten.
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
-- Fall 4: Rightchild des Rightchild ist rot
-- Der rechte Teilbaum hat ein rotes Child (y), dessen eigenes Child (z) rot ist.
-- Lösung: Rotationsumstrukturierung und Umfärbung der Knoten.
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)


-- Standardfall: Keine Umstrukturierung erforderlich
-- Wenn keine der oben genannten Regeln verletzt wird, bleibt der Knoten unverändert.
balance color left val right = Node color left val right




-- In-order traversal 
traverseInOrder :: RBTree a -> [a]
traverseInOrder Empty = []
traverseInOrder (Node _ left val right) =
    traverseInOrder left ++ [val] ++ traverseInOrder right

-- Write sorted list to "output.txt"
writeToFile :: FilePath -> [String] -> IO ()
writeToFile filePath wordsList =
    writeFile filePath (unlines wordsList)

-- Hauptprogramm
main :: IO ()
main = do
    putStrLn "Enter the file path:"
    filePath <- getLine

    -- Step 1: Messen des Einlesens
    content <- measureTime (readFile filePath)

    -- Step 2: Messen des Tokenisierens
    wordsList <- measurePureTime (tokenize content)

    -- Step 3: Messen des Einfügens in den Baum
    tree <- measurePureTime (foldr insert Empty wordsList)

    -- Step 4: Messen des Traversierens
    sortedWords <- measurePureTime (traverseInOrder tree)

    -- Step 5: Schreiben in die Datei (ohne Messung)
    measureTime (writeToFile "output.txt" sortedWords)

    putStrLn "Words sorted and written to output.txt."
