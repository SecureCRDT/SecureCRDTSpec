{-# LANGUAGE TypeFamilies, StandaloneDeriving, TypeOperators, ScopedTypeVariables, TupleSections #-}

-- | General utility functions.
module Secure.CRDT.Utils where

import Data.Typeable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Foldable
import Data.Maybe
import Data.Functor.Compose
import Data.Functor.Product
import Control.Monad
import Control.Concurrent.MVar

-- * Distributed abstractions

-- | Replica id
type R = Int

-- | Party id
type P = Int

-- | Replica-local time
newtype T = T Integer deriving (Eq,Ord,Show)
startT :: T
startT = T 0
nextT :: T -> T
nextT (T t) = T $ succ t

-- | Lamport clock
data LC = LC T R deriving (Eq,Ord,Show)

-- | Vector clock
type VC = Map R T

leVC :: VC -> VC -> Bool
leVC vv1 vv2 = all id $ Map.mergeWithKey (\k i1 i2 -> Just (i1 <= i2)) (Map.map (const False)) (Map.map (const True)) vv1 vv2

incVC :: R -> VC -> VC
incVC i = Map.alter (Just . nextT . fromMaybe startT) i

lookupVC :: R -> VC -> T
lookupVC i vc = maybe startT id (Map.lookup i vc)

maxVC :: VC -> VC -> VC
maxVC vc1 vc2 = Map.unionWith max vc1 vc2

maxVCs :: [VC] -> VC
maxVCs = Map.unionsWith max

-- | Matrix clock
type MC = Map R VC

mergeMC :: R -> VC -> MC -> MC
mergeMC j vc_j mc = Map.alter (Just . maybe vc_j (maxVC vc_j)) j mc

lookupMC :: R -> MC -> VC
lookupMC i mc = maybe Map.empty id (Map.lookup i mc)

leMC :: LC -> MC -> Bool
leMC (LC c i) mc = Map.foldrWithKey (\j vc_j b -> b && c <= lookupVC i vc_j) True mc

emptyMC :: MC
emptyMC = Map.empty

-- * Monadic Maybe interface

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM mz Nothing = mz
fromMaybeM mz (Just a) = return a

-- * Monad List interface

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq [] = return []
nubByM eq (x:xs) = filterM (\y -> liftM not (eq x y)) xs >>= liftM (x:) . nubByM eq

-- * Monadic Map interface
    
mlookup :: Ord k => k -> Map k a -> a
mlookup k = maybe (error "mlookup") id . Map.lookup k

mzip :: Ord k => Map k a -> Map k b -> Map k (a,b)
mzip = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched $ \k a b -> (a,b))

fromSetM :: (Ord k,Monad m) => (k -> m a) -> Set k -> m (Map k a)
fromSetM f s = liftM Map.fromList $ forM (Set.toList s) $ \k -> liftM (k,) (f k)

mfoldrM :: Monad m => (a -> b -> m b) -> m b -> Map k a -> m b
mfoldrM f mz m = mz >>= \z -> foldrM f z m

mfoldrWithKeyM :: Monad m => (k -> a -> b -> m b) -> m b -> Map k a -> m b
mfoldrWithKeyM f mz m = mz >>= \z -> foldrM (\(k,a) b -> f k a b) z (Map.toList m)

mmapWithKeyM :: Monad m => (k -> a -> m a) -> Map k a -> m (Map k a)
mmapWithKeyM f m = forM (Map.mapWithKey f m) id

malterM :: (Ord k,Monad m) => (Maybe a -> m (Maybe a)) -> k -> Map k a -> m (Map k a)
malterM f k m = liftM (maybe m (\v -> Map.insert k v m)) $ f (Map.lookup k m) 

munionWithM :: (Ord k,Monad m) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
munionWithM f ma mb = forM mab id
    where
    ret = Map.mapMissing (const return)
    mab = Map.merge ret ret (Map.zipWithMatched (const f)) ma mb

-- * Miscellaneous

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x >= f y then x else y

writeMVar :: MVar a -> a -> IO ()
writeMVar m v = swapMVar m v >> return ()

deriving instance Show (f (g a)) => Show (Compose f g (a :: * -> *))
deriving instance (Show (f a),Show (g a)) => Show (Product f g (a :: * -> *))

-- | Functor type conversion
fcast :: forall r1 r2 (a :: * -> *). (Typeable r1,Typeable r2) => Proxy r2 -> r1 a -> Maybe (r2 a)
fcast (_::Proxy r2) (r::r1 a) = case (eqT :: Maybe (r1 :~: r2)) of
    Just Refl -> Just r
    Nothing -> Nothing
