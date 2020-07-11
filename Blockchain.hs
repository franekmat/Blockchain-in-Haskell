module Blockchain where
import Control.Monad
import Data.Word
import Data.Maybe

import Hashable32
import HashTree
import PPrint
import Utils

type Address = Hash
type Amount = Word32
coin :: Amount
coin = 1000
data Transaction = Tx
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Amount
  } deriving Show

instance Hashable Transaction where
  hash (Tx a b c) = hash [hash a, hash b, hash c]

data Block = Block { blockHdr :: BlockHeader, blockTxs ::  [Transaction]}

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader
  {
    parent :: Hash
  , coinbase :: Transaction
  , txroot :: Hash -- root of the Merkle tree
  , nonce :: Hash
  } deriving Show

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p,hash c, r, n]

difficulty = 5
blockReward = 50*coin
coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

validNonce :: BlockHeader -> Bool
validNonce b = (hash b) `mod` (2^difficulty) == 0

tx1 = Tx
  { txFrom = hash "Alice"
  , txTo = hash "Bob"
  , txAmount = 1*coin
  }

type Miner = Address
type Nonce = Word32

mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txs = Block (getBlockHeader miner parent txs 0) txs
  where
    getBlockHeader :: Miner -> Hash -> [Transaction] -> Hash -> BlockHeader
    getBlockHeader miner parent txs k
      | (validNonce tryBlockHeader) == True = tryBlockHeader
      | otherwise                           = getBlockHeader miner parent txs (k + 1)
        where
          tryBlockHeader = BlockHeader parent (coinbaseTx miner)
                                       (treeHash (buildTree ((coinbaseTx miner) : txs))) k

genesis = block0
block0 = mineBlock (hash "Satoshi") 0 []
block1 = mineBlock (hash "Alice") (hash genesis) []
block2 = mineBlock (hash "Charlie") (hash block1) [tx1]
chain = [block2, block1, block0]

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380

validChain :: [Block] -> Bool
validChain blocks = not (isNothing (verifyChain blocks))

verifyChain :: [Block] -> Maybe Hash
verifyChain blocks = do
  guard (isNothing (foldl verifyMaybeBlock (maybeBlock (head (reverse blocks)))
                    (tail (reverse blocks))) == False)
  return (hash (head blocks))
    where
      maybeBlock :: Block -> Maybe Block
      maybeBlock block = do
        return block
      verifyMaybeBlock :: Maybe Block -> Block -> Maybe Block
      verifyMaybeBlock x b@(Block hdr txs) = do
        guard (parent hdr == (hash (Data.Maybe.fromJust x)))
        guard (validNonce hdr)
        guard (txFrom (coinbase hdr) == 0 && txAmount (coinbase hdr) == blockReward)
        guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs)))
        return b

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (validNonce hdr)
  guard (txFrom (coinbase hdr) == 0 && txAmount (coinbase hdr) == blockReward)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs)))
  return (hash b)


{- | Transaction Receipts
>>> let charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
<BLANKLINE>

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}
>>> validateReceipt receipt (blockHdr block)
True
-}

data TransactionReceipt = TxReceipt
  {  txrBlock :: Hash, txrProof :: MerkleProof Transaction } deriving Show

validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt r hdr = txrBlock r == hash hdr
                        && verifyProof (txroot hdr) (txrProof r)

mineBlock2 :: Miner -> Hash -> [Transaction] -> (Block, Tree Transaction)
mineBlock2 miner parent txs = (Block (getBlockH miner parent txs 0) txs, tree)
  where
    tree = buildTree ((coinbaseTx miner) : txs)
    getBlockH :: Miner -> Hash -> [Transaction] -> Hash -> BlockHeader
    getBlockH miner parent txs k
      | (validNonce tryBlockHeader) == True = tryBlockHeader
      | otherwise                           = getBlockH miner parent txs (k + 1)
        where
          tryBlockHeader = BlockHeader parent (coinbaseTx miner) (treeHash (tree)) k

mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txs = (block, receipts (hash (blockHdr block)) txs)
  where
    (block, tree) = mineBlock2 miner parent txs
    receipts :: Hash -> [Transaction] -> [TransactionReceipt]
    receipts h txs = map (TxReceipt h) (map getProof txs)
      where
        getProof :: Transaction -> MerkleProof Transaction
        getProof a = MerkleProof a (head (merklePaths a tree))

{- | Pretty printing
>>> runShows $ pprBlock block2
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprListWith pprBlock [block0, block1, block2]
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000
-}
pprHeader :: BlockHeader -> ShowS
pprHeader self@(BlockHeader parent cb txroot nonce)
  = pprV [ p ("hash", VH $ hash self)
         , p ("parent", VH $ parent)
         , p ("miner", VH $ txTo cb)
         , p ("root", VH txroot)
         , p ("nonce", nonce)
         ]
  where
    nl = showString "\n"
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprBlock :: Block -> ShowS
pprBlock (Block header txs)
 = pprHeader header
 . showString "\n"
 . pprTxs (coinbase header:txs)

pprTx :: Transaction -> ShowS
pprTx tx@(Tx from to amount)
  = pprH [ showString "Tx#"
         , showsHash (hash tx)
         , p ("from", VH from)
         , p ("to", VH to)
         , p ("amount", amount)
         ]
  where
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprTxs :: [Transaction] -> ShowS
pprTxs = pprV . map pprTx
