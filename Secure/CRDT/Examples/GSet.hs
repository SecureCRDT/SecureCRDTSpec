{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure grow-only set.
module Secure.CRDT.Examples.GSet where

import Secure.CRDT

import Prelude hiding (seq)
import Data.Foldable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Control.Monad
import Control.Monad.IO.Class

data GSet (a :: *)
instance (SBool w,SEq w a) => CRDT (GSet a) w where
    data State (GSet a) w = State_GSet [S a w]
    data Update (GSet a) w = Add_GSet (S a w)
    data Query (GSet a) r w where
        GetAll_GSet :: Query (GSet a) ([] `Compose` S a) w
        Exists_GSet :: S a w -> Query (GSet a) (S Bool) w
    data Message (GSet a) w = Message_GSet [S a w]
    
    new i = return $ State_GSet []
    query i GetAll_GSet (State_GSet st) = return $ Compose st
    query i (Exists_GSet x) (State_GSet ys) = do
        z <- classify False
        foldrM (\y b -> seq x y >>= (`sor` b)) z ys
    update i (Add_GSet x) (State_GSet st) = liftM State_GSet $ insert_GSet x st
    propagate i j (State_GSet st_i) = return $ Message_GSet st_i
    merge i j (Message_GSet m_i) (State_GSet st_j) = liftM State_GSet $ foldrM insert_GSet st_j m_i

insert_GSet :: (SBool w,SEq w a) => S a w -> [S a w] -> w [S a w]
insert_GSet x ys = do
    z <- classify False
    b <- foldrM (\y b -> seq x y >>= (`sor` b)) z ys >>= declassify
    return $ if b then ys else x : ys

instance SZip (Query (GSet a) ([] `Compose` S a)) where
    sunzip GetAll_GSet = Map.fromSet (const GetAll_GSet) psRW
    szip ss = GetAll_GSet
    
instance SZip (Query (GSet a) (S Bool)) where
    sunzip (Exists_GSet s) = Map.map (Exists_GSet) (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' :: Query (GSet a) (S Bool) RW -> [(P,Query (GSet a) (S Bool) RW)] -> Query (GSet a) (S Bool) IW
        szip' (Exists_GSet _) xs = Exists_GSet $ szip $ szip_e xs
        szip_e :: [(P,Query (GSet a) (S Bool) RW)] -> Map P (S a RW)
        szip_e [] = Map.empty
        szip_e ((p,Exists_GSet v):xs) = Map.insert p v (szip_e xs)

instance SZip (Update (GSet a)) where
    sunzip (Add_GSet s) = fmap Add_GSet (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Add_GSet _) xs = Add_GSet (szip $ szip_add xs)
        szip_add [] = Map.empty
        szip_add ((p,Add_GSet v):xs) = Map.insert p v (szip_add xs)

---------------------------

testGSet :: IO ()
testGSet = run 3 testGSet'

testGSet' :: Client (GSet Int) RW ()
testGSet' = do
    cupdate 0 $ Add_GSet (SIW 5) 
    cupdate 1 $ Add_GSet (SIW 3)
    cquery 0 GetAll_GSet >>= liftIO . print
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 (Exists_GSet (SIW 5)) >>= liftIO . print
