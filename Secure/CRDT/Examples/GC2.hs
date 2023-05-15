{-# LANGUAGE TypeFamilies, GADTs, FlexibleContexts, UndecidableInstances, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure grow-only counter with timestamp comparison.
module Secure.CRDT.Examples.GC2 where

import Secure.CRDT

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class

data GC2
instance (World w,SNum w Int,SOrd w Int) => CRDT GC2 w where
    data State GC2 w = State_GC2 (Map R (S Int w,T))
    data Update GC2 w = Inc_GC2 { inc_GC2 :: S Int w } 
    data Query GC2 r w where
        Get_GC2 :: Query GC2 (S Int) w
    data Message GC2 w = Message_GC2 (Map R (S Int w,T))
    
    new i = return $ State_GC2 $ Map.empty
    query i Get_GC2 (State_GC2 st) = mfoldrM (\(n,_) -> splus n) (classify 0) st
    update i (Inc_GC2 n) (State_GC2 st) = liftM State_GC2 $ malterM (liftM Just . maybe (return (n,startT)) (\(n',t') -> liftM (,nextT t') (splus n n'))) i st
    propagate i j (State_GC2 st_i) = return $ Message_GC2 $ maybe Map.empty (Map.singleton i) (Map.lookup i st_i)
    merge i j (Message_GC2 m_i) (State_GC2 st_j) = return $ State_GC2 $ Map.unionWith (maxOn snd) m_i st_j

instance SZip (Query GC2 (S Int)) where
    sunzip Get_GC2 = Map.fromSet (const Get_GC2) psRW
    szip ss = Get_GC2

instance SZip (Update GC2) where
    sunzip (Inc_GC2 s) = fmap Inc_GC2 (sunzip s)
    szip ss = Inc_GC2 $ szip (Map.map inc_GC2 ss)

---------------------------

testGC2 :: IO ()
testGC2 = run 3 testGC2'

testGC2' :: Client GC2 RW ()
testGC2' = do
    cupdate 0 $ Inc_GC2 (SIW 3)
    cpropagate 0 1
    cupdate 1 $ Inc_GC2 (SIW 2)
    cmerge 0 1
    cquery 1 Get_GC2 >>= liftIO . print