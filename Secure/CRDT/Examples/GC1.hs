{-# LANGUAGE TypeFamilies, GADTs, FlexibleContexts, UndecidableInstances, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure grow-only counter.
module Secure.CRDT.Examples.GC1 where

import Secure.CRDT

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class

data GC1
instance (World w,SNum w Int,SOrd w Int) => CRDT GC1 w where
    data State GC1 w = State_GC1 (Map R (S Int w))
    data Update GC1 w = Inc_GC1 { inc_GC1 :: S Int w } 
    data Query GC1 r w where
        Get_GC1 :: Query GC1 (S Int) w
    data Message GC1 w = Message_GC1 (Map R (S Int w)) 
    
    new i = return $ State_GC1 $ Map.empty
    query i Get_GC1 (State_GC1 st) = mfoldrM splus (classify 0) st
    update i (Inc_GC1 n) (State_GC1 st) = liftM State_GC1 $ malterM (liftM Just . maybe (return n) (splus n)) i st
    propagate i j (State_GC1 st_i) = return $ Message_GC1 $ maybe Map.empty (Map.singleton i) (Map.lookup i st_i)
    merge i j (Message_GC1 m_i) (State_GC1 st_j) = liftM State_GC1 $ munionWithM smax m_i st_j

instance SZip (Query GC1 (S Int)) where
    sunzip Get_GC1 = Map.fromSet (const Get_GC1) psRW
    szip ss = Get_GC1

instance SZip (Update GC1) where
    sunzip (Inc_GC1 s) = fmap Inc_GC1 (sunzip s)
    szip ss = Inc_GC1 $ szip (Map.map inc_GC1 ss)

---------------------------

testGC1 :: IO ()
testGC1 = run 3 testGC1'

testGC1' :: Client GC1 RW ()
testGC1' = do
    cupdate 0 $ Inc_GC1 (SIW 3)
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 Get_GC1 >>= liftIO . print