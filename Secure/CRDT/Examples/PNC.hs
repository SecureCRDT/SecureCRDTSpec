{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, UndecidableInstances, TupleSections, FlexibleContexts, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure positive-negative counter.
module Secure.CRDT.Examples.PNC where

import Secure.CRDT
import Secure.CRDT.Examples.GC1

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

data PNC
instance (World w,SNum w Int,SOrd w Int) => CRDT PNC w where
    data State PNC w = State_PNC (State GC1 w,State GC1 w)
    data Update PNC w = Inc_PNC { inc_PNC :: S Int w } | Dec_PNC { dec_PNC :: S Int w }
    data Query PNC r w where
        Get_PNC :: Query PNC (S Int) w
    data Message PNC w = Message_PNC (Message GC1 w,Message GC1 w)
    
    new i = liftM State_PNC $ liftA2 (,) (new i) (new i)
    query i Get_PNC (State_PNC (pst,nst)) = do
        r1 <- query i Get_GC1 pst
        r2 <- query i Get_GC1 nst
        sminus r1 r2
    update i (Inc_PNC n) (State_PNC (pst,nst)) = do
        liftM (State_PNC . (,nst)) (update i (Inc_GC1 n) pst)
    update i (Dec_PNC n) (State_PNC (pst,nst)) = do
        liftM (State_PNC . (pst,)) (update i (Inc_GC1 n) nst)
    propagate i j (State_PNC (pst_i,nst_i)) = do
        liftM Message_PNC $ liftA2 (,) (propagate i j pst_i) (propagate i j nst_i)
    merge i j (Message_PNC (pm_i,nm_i)) (State_PNC (pst_j,nst_j)) = do
        liftM State_PNC $ liftA2 (,) (merge i j pm_i pst_j) (merge i j nm_i nst_j)

instance SZip (Query PNC (S Int)) where
    sunzip Get_PNC = Map.fromSet (const Get_PNC) psRW
    szip ss = Get_PNC

instance SZip (Update PNC) where
    sunzip (Inc_PNC s) = fmap Inc_PNC (sunzip s)
    sunzip (Dec_PNC s) = fmap Dec_PNC (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Inc_PNC _) xs = Inc_PNC $ szip $ szip_inc xs
        szip' (Dec_PNC _) xs = Dec_PNC $ szip $ szip_dec xs
        szip_inc [] = Map.empty
        szip_inc ((p,Inc_PNC v):xs) = Map.insert p v (szip_inc xs)
        szip_inc (x:xs) = szip_inc xs
        szip_dec [] = Map.empty
        szip_dec ((p,Dec_PNC v):xs) = Map.insert p v (szip_dec xs)
        szip_dec (x:xs) = szip_dec xs

---------------------------

testPNC :: IO ()
testPNC = run 3 testPNC'

testPNC' :: Client PNC RW ()
testPNC' = do
    cupdate 0 $ Inc_PNC (SIW 3)
    cpropagate 0 1
    cupdate 1 $ Dec_PNC (SIW 1)
    cmerge 0 1
    cquery 1 Get_PNC >>= liftIO . print

