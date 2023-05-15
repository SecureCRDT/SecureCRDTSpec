{-# LANGUAGE TypeFamilies, TypeOperators, ScopedTypeVariables, GADTs, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

-- | Input/output interface for CRDT clients.
module Secure.CRDT.IO where

import Secure.CRDT.Utils
import Secure.CRDT.World.Ideal
import Secure.CRDT.World.Real
import Secure.CRDT.Class

import Data.Typeable
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Product
import Control.Functor.Zip

-- | Typed interface for client inputs.
data Input n w where
    New :: Input n w -- ^ for internal use only, not exposed to clients. 
    Query :: (Typeable r,SZip (Query n r),SZip r) => Query n r w -> Input n w
    Update :: SZip (Update n) => Update n w -> Input n w
    Propagate :: R -> Input n w
    Merge :: R -> Input n w

-- | Typed interface for client outputs.
data Output n w where
    Out :: (Typeable r,SZip (Query n r),SZip r) => Query n r w -> r w -> Output n w

-- | Secret-specific functor zipping class.
class SZip f where
    -- | maps a structure with values in the ideal world to an unzipped structure with shares per party in the real world
    sunzip :: f IW -> Map P (f RW)
    -- | maps an unzipped structure of shares per party in the real world to a structure with values in the ideal world
    szip   :: Map P (f RW) -> f IW
    
-- | Conversion between ideal inputs/outputs seen by the client and inputs/outputs in the simulated world.
class (CRDT n w) => SIO n w where
    sinput :: Input n IW -> Map P (Input n w)
    soutput :: Map P (Output n w) -> Output n IW
    
-- | The core instance for secret data.
-- All other instance should simply map over this instance, and could be automatically derived.
-- NOTE: Like all real-world MPC in this library, this instance assumes dummy MPC parties that keep copies of the original value!
instance SZip (S a) where
    sunzip (SIW s) = Map.fromSet (const $ SRW s) psRW
    szip ss = case Map.lookup 0 ss of
        Just (SRW s) -> SIW s

-- | Instance for input type, needed for sharing client inputs among MPC parties.
-- More complex type handling because of GADT encapsulation.
instance SZip (Input n) where
    sunzip New = Map.fromSet (const New) psRW
    sunzip (Query q) = Map.map Query (sunzip q)
    sunzip (Update u) = Map.map Update (sunzip u)
    sunzip (Propagate j) = Map.fromSet (const $ Propagate j) psRW
    sunzip (Merge j) = Map.fromSet (const $ Merge j) psRW  
    szip m = szip' (mlookup 0 m) (Map.toList m)
        where
        szip' :: Input n RW -> [(P,Input n RW)] -> Input n IW
        szip' New _ = New
        szip' (Query (q :: Query n r RW)) xs = Query $ szip (unQueries (Proxy::Proxy r) xs)
            where
            unQueries :: Typeable r => Proxy r -> [(P,Input n w)] -> Map P (Query n r w)
            unQueries r [] = Map.empty
            unQueries r ((p,x):xs) = let ys = unQueries r xs in maybe ys (\y -> Map.insert p y ys) (unQuery r x)
            unQuery :: Typeable r => Proxy r -> Input n w -> Maybe (Query n r w)
            unQuery _ (Query (q::Query n r1 w)) = case (eqT :: Maybe (r :~: r1)) of
                Just Refl -> Just q
                Nothing -> Nothing
            unQuery _ _ = Nothing
        szip' (Update u) xs = Update $ szip (unUpdates xs)
            where
            unUpdates :: [(P,Input n w)] -> Map P (Update n w)
            unUpdates [] = Map.empty
            unUpdates ((p,Update u):xs) = Map.insert p u (unUpdates xs)
            unUpdates (x:xs) = unUpdates xs
        szip' (Propagate j) xs = Propagate j
        szip' (Merge j) xs = Merge j

-- | Instance for output type, needed for unsharing client outputs among MPC parties.
-- More complex type handling because of GADT encapsulation.
instance SZip (Output n) where
    sunzip (Out q r) = Map.map (\(q,r) -> Out q r) $ mzip (sunzip q) (sunzip r)
    szip m = szip' (mlookup 0 m) (Map.toList m)
        where
        szip' :: Output n RW -> [(P,Output n RW)] -> Output n IW
        szip' (Out (q::Query n r RW) (r::r RW)) xs = Out (szip $ unQueries r xs) (szip $ unRes r xs)
            where
            r = Proxy :: Proxy r
            unQueries :: Typeable r => Proxy r -> [(P,Output n w)] -> Map P (Query n r w)
            unQueries r [] = Map.empty
            unQueries r ((p,x):xs) = let ys = unQueries r xs in maybe ys (\y -> Map.insert p y ys) (unQuery r x)
            unQuery :: Typeable r => Proxy r -> Output n w -> Maybe (Query n r w)
            unQuery _ (Out (q::Query n r1 w) _) = case (eqT :: Maybe (r :~: r1)) of
                Just Refl -> Just q
                Nothing -> Nothing
            unRes :: Typeable r => Proxy r -> [(P,Output n w)] -> Map P (r w)
            unRes r [] = Map.empty
            unRes r ((p,x):xs) = let ys = unRes r xs in maybe ys (\y -> Map.insert p y ys) (unRe r x)
            unRe :: Typeable r => Proxy r -> Output n w -> Maybe (r w)
            unRe _ (Out _ (r::r1 w)) = case (eqT :: Maybe (r :~: r1)) of
                Just Refl -> Just r
                Nothing -> Nothing
    
instance (Zip f,SZip g) => SZip (f `Compose` g) where
    sunzip (Compose fgx) = Map.fromSet (\p -> Compose $ fmap (mlookup p) (fmap sunzip fgx)) psRW 
    szip m = Compose $ fmap szip $ fzipMap $ Map.map getCompose m

instance SZip (Const c) where
    sunzip (Const c) = Map.fromSet (const $ Const c) psRW
    szip m = case mlookup 0 m of
        Const c -> Const c
    
instance (SZip f,SZip g) => SZip (f `Product` g) where
    sunzip (Pair fx gx) = Map.map (uncurry Pair) $ mzip (sunzip fx) (sunzip gx)
    szip m = Pair (szip $ Map.map (\(Pair x y) -> x) m) (szip $ Map.map (\(Pair x y) -> y) m)
    
-- | Direct mapping between client inputs/outputs and ideal-world inputs/outputs.
-- In the ideal world, there is only one party.
instance (CRDT n IW) => SIO n IW where
    sinput i = Map.singleton 0 i
    soutput m = mlookup 0 m
    
-- | Mapping between client inputs/outputs and real-world inputs/outputs.
-- Binding to our specific secret zipping class.
instance (CRDT n RW,SZip (Input n),SZip (Output n)) => SIO n RW where
    sinput = sunzip
    soutput = szip
