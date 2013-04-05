module Bench where

import System.CPUTime
import Text.Printf

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad

import System.Time
import System.CPUTime
import System.Random
-- import Data.Tree.AVL
-- import Data.COrdering
import System.Environment
import System.Mem
import Data.Typeable
import Control.Monad.Trans
import Data.Tree
import Data.List
import qualified Data.Map as M

type Context = [String]

type Measure = (Context, Integer)

type Benchmark = ReaderT Context (WriterT [Measure] IO)


blift :: IO a -> Benchmark a
blift = lift . lift

__ = undefined

withLab :: String -> Benchmark a -> Benchmark a
withLab label = local (++[label])

timeIO :: IO a -> Benchmark a
timeIO value = do 
     t1 <- blift getCPUTime
     blift performGC
     t2 <- blift getCPUTime
     result <- blift value
     t3 <- blift getCPUTime
     blift performGC
     t4 <- blift getCPUTime
     --withLab "GC before" $ report (t2-t1)
     --withLab "computation" $ 
     report (t3-t2)
     --withLab "GC after" $ report (t4-t3)
     return result
    
report :: Integer -> Benchmark () 
report time = do ctx <- ask
                 tell [(ctx, time)]
                 return ()

withType :: Typeable t => t -> Benchmark a -> Benchmark a
withType t = withLab (show $ typeOf t)

many :: Int -> Benchmark a -> Benchmark ()
many n bench = sequence_ (replicate n bench)

data Trie k v = Trie (Maybe v) (M.Map k (Trie k v))

toTrie :: Ord k => ([k],v) -> Trie k v
toTrie ([],v) = Trie (Just v) (M.empty)
toTrie ((k:ks),v) = Trie Nothing (M.singleton k (toTrie (ks,v)))

instance (Ord k, Monoid v) => Monoid (Trie k v) where
    mempty = Trie Nothing M.empty
    mappend (Trie v0 s0) (Trie v1 s1) = Trie (v0 `mappend` v1) (M.unionWith mappend s0 s1)

toTree :: (Show l, Show k, Show v) => l -> Trie k v -> Tree String
toTree rootLabel (Trie v m) = Node (show rootLabel ++ " -> "++ show v) (map (uncurry toTree) (M.assocs m))

-- toList :: (Show k, Show v) -> Trie k v -> [[String]]


fromList :: (Ord k, Monoid v) => [([k],v)] -> Trie k v
fromList = mconcat . map toTrie


runBenchmark :: Benchmark a -> IO a
runBenchmark bench = do (a, results) <- runWriterT (runReaderT bench []) 
                        let t = fromList [(l,[m]) | (l,m) <- results]
                            t :: Trie String [Integer]
                            tree = toTree () t 
                        putStrLn $ drawTree $ tree
                        forM results $ \(context, v) -> do
                            printf "%20d %s\n" v (show context)
                        return a

average list = sum list `div` genericLength list 

showNode (label, Nothing) = label
showNode (label, Just x) = label ++ " -> " ++ show (average x)