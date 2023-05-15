{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, FlexibleContexts, UndecidableInstances, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure bounded counter counter.
module Secure.CRDT.Examples.BC where

import Secure.CRDT

import Data.Foldable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

data BC
instance (World w,SNum w Int,SOrd w Int) => CRDT BC w where
    data State BC w = State_BC (S Int w,Map (R,R) (S Int w,T),Map R (S Int w,T))
    data Update BC w
        = Inc_BC (S Int w)
        | Dec_BC (S Int w)
        | Transf_BC (S Int w) R
    data Query BC r w where
        Get_BC :: Query BC (S Int) w
    data Message BC w = Message_BC (S Int w,Map (R,R) (S Int w,T),Map R (S Int w,T))
    
    new i = liftM (State_BC . (,Map.empty,Map.empty)) (classify 0) -- minimum value=0
    query i Get_BC st@(State_BC (k,r,u)) = do
        p <- mfoldrWithKeyM (\(k1,k2) (n,_) -> if k1==k2 then splus n else return) (return k) r
        foldrM (\(n,_) b -> sminus b n) p u
    update i (Inc_BC n) (State_BC (k,r,u)) = do
        let f mb = do
                (n',t') <- fromMaybeM (liftM (,startT) $ classify 0) mb
                n'' <- splus n n'
                return $ Just $ (n'',nextT t')
        r' <- malterM f (i,i) r
        return $ State_BC (k,r',u)
    update i (Dec_BC n) st@(State_BC (k,r,u)) = do
        lr <- localRights_BC i st
        let f mb = do
                (n',t') <- fromMaybeM (liftM (,startT) $ classify 0) mb
                sge lr n >>= \b -> splus n n' >>= \n'' -> liftM (Just . (,nextT t')) $ sif b n'' n'
        u' <- malterM f i u
        return $ State_BC (k,r,u')
    update i (Transf_BC n j) st@(State_BC (k,r,u)) = do
        lr <- localRights_BC i st
        let f mb = do
                (n',t') <- fromMaybeM (liftM (,startT) $ classify 0) mb
                sge lr n >>= \b -> splus n n' >>= \n'' -> liftM (Just . (,nextT t')) $ sif b n'' n'
        r' <- malterM f (i,j) r 
        return $ State_BC (k,r',u)
    propagate i j (State_BC st_i) = return $ Message_BC st_i
    merge i j (Message_BC (k,r_i,u_i)) (State_BC (_,r_j,u_j)) = return $ State_BC (k,r_i',u_i')
        where
        r_i' = Map.unionWith (maxOn snd) r_i r_j
        u_i' = Map.unionWith (maxOn snd) u_i u_j

localRights_BC :: SNum w Int => R -> State BC w -> w (S Int w)
localRights_BC i (State_BC (k,r,u)) = do
    r_r <- mfoldrWithKeyM (\(k1,k2) (n,_) -> if k2==i then splus n else return) (classify 0) r
    r_s <- mfoldrWithKeyM (\(k1,k2) (n,_) -> if k1==i then splus n else return) (classify 0) r
    p <- maybe (classify 0) (return . fst) (Map.lookup (i,i) r)
    n <- maybe (classify 0) (return . fst) (Map.lookup i u)
    p `splus` r_r >>= (`sminus` r_s) >>= (`sminus` n)

instance SZip (Query BC (S Int)) where
    sunzip Get_BC = Map.fromSet (const Get_BC) psRW
    szip ss = Get_BC

instance SZip (Update BC) where
    sunzip (Inc_BC s) = fmap Inc_BC (sunzip s)
    sunzip (Dec_BC s) = fmap Dec_BC (sunzip s)
    sunzip (Transf_BC s r) = fmap (\s -> Transf_BC s r) (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Inc_BC _) xs = Inc_BC $ szip $ szip_inc xs
        szip' (Dec_BC _) xs = Dec_BC $ szip $ szip_dec xs
        szip' (Transf_BC _ r) xs = Transf_BC (szip $ szip_trf xs) r
        szip_inc [] = Map.empty
        szip_inc ((p,Inc_BC v):xs) = Map.insert p v (szip_inc xs)
        szip_inc (x:xs) = szip_inc xs
        szip_dec [] = Map.empty
        szip_dec ((p,Dec_BC v):xs) = Map.insert p v (szip_dec xs)
        szip_dec (x:xs) = szip_dec xs
        szip_trf [] = Map.empty
        szip_trf ((p,Transf_BC v _):xs) = Map.insert p v (szip_trf xs)
        szip_trf (x:xs) = szip_trf xs

---------------------------

testBC :: IO ()
testBC = run 3 testBC'

testBC' :: Client BC RW ()
testBC' = do
    cupdate 0 $ Inc_BC (SIW 3)
    cupdate 0 $ Transf_BC (SIW 2) 1
    cpropagate 0 1
    cmerge 0 1
    cupdate 1 $ Dec_BC (SIW 1)
    cquery 1 Get_BC >>= liftIO . print
