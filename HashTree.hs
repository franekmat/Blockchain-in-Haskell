module HashTree where
import Hashable32
import Utils
import Data.Foldable

data Tree a = Leaf Hash a
  | Node Hash (Tree a) (Tree a)
  | Twig Hash (Tree a)
  deriving Show

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
  showsPrec p (MerkleProof a path) = showParen (p > 10) $
    showString "MerkleProof "
      . showsPrec 11 a
      . showString " "
      . showString (showMerklePath path)

treeHash :: Tree a -> Hash
treeHash (Leaf n _) = n
treeHash (Node n _ _) = n
treeHash (Twig n _) = n

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

node :: Hashable a => Tree a -> Tree a -> Tree a
node a b = Node (combine (treeHash a) (treeHash b)) a b

twig :: Hashable a => Tree a -> Tree a
twig a = Twig (combine (treeHash a) (treeHash a)) a

buildTree :: Hashable a => [a] -> Tree a
buildTree []   = error "empty list"
buildTree list = buildTreeHelp (map leaf list)
  where
    buildTreeHelp :: Hashable a => [Tree a] -> Tree a
    buildTreeHelp list
      | length list == 1 = head list
      | otherwise        = buildTreeHelp (gen list)
        where
          gen :: Hashable a => [Tree a] -> [Tree a]
          gen []  = []
          gen [a] = [twig a]
          gen (a : b : cd) = (node a b) : (gen cd)

drawTree :: Show a => Tree a -> String
drawTree a = drawTreeHelp a ""
  where
    drawTreeHelp :: Show a => Tree a -> String -> String
    drawTreeHelp (Leaf n v) s = s ++ showHash(n) ++ " " ++ show v ++ "\n"
    drawTreeHelp (Node n a b) s = s ++ showHash(n) ++ " -\n" ++ drawTreeHelp a (s ++ " ")
                                  ++ drawTreeHelp b (s ++ " ")
    drawTreeHelp (Twig n a) s = s ++ showHash(n) ++ " +\n" ++ drawTreeHelp a (s ++ " ")

showMerklePath :: MerklePath -> String
showMerklePath path = connect (map showMerklePathHelp path)
  where
    connect :: [String] -> String -- https://stackoverflow.com/a/42360812
    connect []    = ""
    connect (x:y) = foldl' (\a b -> a ++ b) x y
    showMerklePathHelp :: Either Hash Hash -> String
    showMerklePathHelp path =
      case path of
        (Left x)  -> "<" ++ showHash(x)
        (Right x) -> ">" ++ showHash(x)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths a t = map reverse (merklePathsH a t [[]])
  where
    merklePathsH :: Hashable a => a -> Tree a -> [MerklePath] -> [MerklePath]
    merklePathsH a (Leaf n v) list
      | n == hash(a) = list
      | otherwise    = []
    merklePathsH a (Twig n x) list = merklePathsH a x [(Left (treeHash x)) : (head list)]
    merklePathsH a (Node n x y) list = merge (merklePathsH a x [(Left (treeHash y)) : (head list)])
                                       (merklePathsH a y [(Right (treeHash x)) : (head list)])
    merge :: [a] -> [a] -> [a]
    merge a []    = a
    merge [] b    = b
    merge (a:b) c = a : (merge c b)

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof a t
  | paths == [] = Nothing
  | otherwise   = Just (MerkleProof a (head paths))
    where
      paths = merklePaths a t

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a path)
  | (foldl solve (hash a) (reverse path)) == h = True
  | otherwise                        = False
    where
      solve :: Hash -> Either Hash Hash -> Hash
      solve n (Left x)  = combine n x
      solve n (Right x) = combine x n
