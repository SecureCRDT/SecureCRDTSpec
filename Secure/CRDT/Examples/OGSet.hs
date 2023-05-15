{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure shuffle grow-only set.
module Secure.CRDT.Examples.OGSet where

import Secure.CRDT
import Prelude hiding (seq)
import Data.Foldable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Data.Int
import Control.Monad
import Control.Monad.IO.Class

data OGSet (a :: *)
instance (Bounded a,Enum a,Ord a,SBool w,SEq w a) => CRDT (OGSet a) w where
    data State (OGSet a) w = State_OGSet (Map a (S Bool w))
    data Update (OGSet a) w = Add_OGSet (S a w)
    data Query (OGSet a) r w where
        GetAll_OGSet :: Query (OGSet a) (Map a `Compose` S Bool) w
        Exists_OGSet :: S a w -> Query (OGSet a) (S Bool) w
    data Message (OGSet a) w = Message_OGSet (Map a (S Bool w))
    
    new i = do
        z <- classify False
        return $ State_OGSet $ Map.fromDistinctAscList $ map (,z) [minBound..maxBound] 
    query i GetAll_OGSet (State_OGSet st) = return $ Compose st
    query i (Exists_OGSet x) (State_OGSet st) = mfoldrWithKeyM (\v b acc -> classify v >>= seq x >>= sor acc) (classify False) st
    update i (Add_OGSet x) (State_OGSet st) = liftM State_OGSet $ mmapWithKeyM (\v b -> classify v >>= seq x >>= sor b) st
    propagate i j (State_OGSet st) = return $ Message_OGSet st
    merge i j (Message_OGSet m_i) (State_OGSet st_j) = liftM State_OGSet $ munionWithM sor st_j m_i

instance SZip (Query (OGSet a) (Map a `Compose` S Bool)) where
    sunzip GetAll_OGSet = Map.fromSet (const GetAll_OGSet) psRW
    szip ss = GetAll_OGSet
    
instance SZip (Query (OGSet a) (S Bool)) where
    sunzip (Exists_OGSet s) = Map.map (Exists_OGSet) (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' :: Query (OGSet a) (S Bool) RW -> [(P,Query (OGSet a) (S Bool) RW)] -> Query (OGSet a) (S Bool) IW
        szip' (Exists_OGSet _) xs = Exists_OGSet $ szip $ szip_e xs
        szip_e :: [(P,Query (OGSet a) (S Bool) RW)] -> Map P (S a RW)
        szip_e [] = Map.empty
        szip_e ((p,Exists_OGSet v):xs) = Map.insert p v (szip_e xs)

instance SZip (Update (OGSet a)) where
    sunzip (Add_OGSet s) = fmap Add_OGSet (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Add_OGSet _) xs = Add_OGSet (szip $ szip_add xs)
        szip_add [] = Map.empty
        szip_add ((p,Add_OGSet v):xs) = Map.insert p v (szip_add xs)

---------------------------

testOGSet :: IO ()
testOGSet = run 3 testOGSet'

testOGSet' :: Client (OGSet Int8) RW ()
testOGSet' = do
    cupdate 0 $ Add_OGSet (SIW 5) 
    cupdate 1 $ Add_OGSet (SIW 3)
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 GetAll_OGSet >>= liftIO . print


