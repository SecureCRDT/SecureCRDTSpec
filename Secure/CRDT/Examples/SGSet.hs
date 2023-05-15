{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure shuffle grow-only set.
module Secure.CRDT.Examples.SGSet where

import Secure.CRDT
import Secure.CRDT.Examples.GSet

import Prelude hiding (seq)
import Data.Foldable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Control.Monad
import Control.Monad.IO.Class

data SGSet (a :: *)
instance (SBool w,SEq w a,SShuffle w a) => CRDT (SGSet a) w where
    data State (SGSet a) w = State_SGSet (State (GSet a) w,Bool)
    data Update (SGSet a) w = Add_SGSet (S a w) | Compress_SGSet
    data Query (SGSet a) r w where
        GetAll_SGSet :: Query (SGSet a) (Maybe `Compose` [] `Compose` S a) w
        Exists_SGSet :: S a w -> Query (SGSet a) (S Bool) w
    data Message (SGSet a) w = Message_SGSet (Message (GSet a) w)
    
    new i = return $ State_SGSet (State_GSet [],True)
    query i GetAll_SGSet (State_SGSet (State_GSet st,False)) = return $ Compose Nothing
    query i GetAll_SGSet (State_SGSet (State_GSet st,True)) = return $ Compose $ Just $ Compose st
    query i (Exists_SGSet x) (State_SGSet (xs,_)) = query i (Exists_GSet x) xs
    update i (Add_SGSet x) (State_SGSet (State_GSet xs,b)) = return $ State_SGSet $ (State_GSet $ x:xs,False)
    update i Compress_SGSet (State_SGSet (State_GSet xs,b)) = do
        xs' <- sshuffle xs >>= nubByM (\x y -> seq x y >>= declassify)
        return (State_SGSet (State_GSet xs',True))
    propagate i j (State_SGSet (State_GSet xs,_)) = return $ Message_SGSet $ Message_GSet xs
    merge i j (Message_SGSet (Message_GSet m_i)) (State_SGSet (State_GSet xs_j,b)) = return $ State_SGSet (State_GSet $ xs_j++m_i,False)

instance SZip (Query (SGSet a) (Maybe `Compose` [] `Compose` S a)) where
    sunzip GetAll_SGSet = Map.fromSet (const GetAll_SGSet) psRW
    szip ss = GetAll_SGSet
    
instance SZip (Query (SGSet a) (S Bool)) where
    sunzip (Exists_SGSet s) = Map.map (Exists_SGSet) (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' :: Query (SGSet a) (S Bool) RW -> [(P,Query (SGSet a) (S Bool) RW)] -> Query (SGSet a) (S Bool) IW
        szip' (Exists_SGSet _) xs = Exists_SGSet $ szip $ szip_e xs
        szip_e :: [(P,Query (SGSet a) (S Bool) RW)] -> Map P (S a RW)
        szip_e [] = Map.empty
        szip_e ((p,Exists_SGSet v):xs) = Map.insert p v (szip_e xs)

instance SZip (Update (SGSet a)) where
    sunzip (Add_SGSet s) = fmap Add_SGSet (sunzip s)
    sunzip Compress_SGSet = Map.fromSet (const Compress_SGSet) psRW
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Add_SGSet _) xs = Add_SGSet (szip $ szip_add xs)
        szip' (Compress_SGSet) xs = Compress_SGSet
        szip_add [] = Map.empty
        szip_add ((p,Add_SGSet v):xs) = Map.insert p v (szip_add xs)

---------------------------

testSGSet :: IO ()
testSGSet = run 3 testSGSet'

testSGSet' :: Client (SGSet Int) RW ()
testSGSet' = do
    cupdate 0 $ Add_SGSet (SIW 5) 
    cupdate 1 $ Add_SGSet (SIW 5)
    cpropagate 0 1 >> cmerge 0 1
    cupdate 1 Compress_SGSet
    cquery 1 GetAll_SGSet >>= liftIO . print
    cpropagate 1 2 >> cmerge 1 2
    cquery 2 (Exists_SGSet (SIW 5)) >>= liftIO . print
