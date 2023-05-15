{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | Ideal world.
module Secure.CRDT.World.Ideal where

import Secure.CRDT.World
import Secure.CRDT.MPC

import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.Identity
import System.Random.Shuffle
import System.IO.Unsafe

-- | Monad for the ideal world where an idealized party keeps secrets as plain values.
type IW = Identity

-- | Ideal world instance.
-- Per replica, there is only a single idealized party. This party simply executes CRDT operations locally and does not need to communicate with other parties, hence the ideal world does not keep any state.
instance World IW where
    ps _ = Set.fromList [0]
    type WorldRef IW = ()
    initWorld _ = return ()
    runWorld _ _ m = return $ runIdentity m 

-- | MPC instance for the ideal world.
instance MPC IW a where
    data S a IW = SIW a deriving Show
    classify a = return (SIW a)
    declassify (SIW a) = return a
    
-- | Numerical MPC instance for the ideal world.
instance Num a => SNum IW a where
    splus (SIW i) (SIW j) = return $ SIW $ i + j
    sminus (SIW i) (SIW j) = return $ SIW $ i - j
    sif (SIW b) x y = return $ if b then x else y
    
-- | Ordering MPC instance for the ideal world.
instance Ord a => SOrd IW a where
    sge (SIW i) (SIW j) = return $ SIW $ i >= j
    smax (SIW i) (SIW j) = return $ SIW $ max i j
    
-- | Equality MPC instance for the ideal world.
instance Eq a => SEq IW a where
    seq (SIW i) (SIW j) = return $ SIW $ i == j
    
-- | Bool MPC instance for the ideal world.
instance SBool IW where
    sor (SIW i) (SIW j) = return $ SIW $ i || j
    sand (SIW i) (SIW j) = return $ SIW $ i && j
    snot (SIW i) = return $ SIW $ not i

instance SShuffle IW a where
    sshuffle xs = return $ unsafePerformIO $ shuffleM xs



