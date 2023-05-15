{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure multi-value register.
module Secure.CRDT.Examples.MVR where

import Secure.CRDT

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Control.Monad
import Control.Monad.IO.Class

data MVR (a :: *)
instance World w => CRDT (MVR a) w where
    data State (MVR a) w = State_MVR (Map VC (S a w))
    data Update (MVR a) w = Upd_MVR (S a w)
    data Query (MVR a) r w where
        Get_MVR :: Query (MVR a) ([] `Compose` S a) w
    data Message (MVR a) w = Message_MVR (Map VC (S a w))
    
    new i = return $ State_MVR Map.empty
    query i Get_MVR (State_MVR st) = return $ Compose $ Map.elems st
    update i (Upd_MVR v) (State_MVR st) = do
        let vv = maxVCs (Map.keys st)
        return $ State_MVR $ merge_MVR' st (Map.singleton (incVC i vv) v)
    propagate i j (State_MVR st_i) = return $ Message_MVR st_i
    merge i j (Message_MVR m_i) (State_MVR st_j) = return $ State_MVR $ merge_MVR' m_i st_j

merge_MVR' :: (Map VC (S a w)) -> (Map VC (S a w)) -> (Map VC (S a w))
merge_MVR' st1 st2 = Map.union st1' st2'
    where
    st1' = Map.filterWithKey (\vv v -> all (not . (leVC vv)) (Map.keys st2)) st1
    st2' = Map.filterWithKey (\vv v -> all (not . (leVC vv)) (Map.keys st1)) st2

instance SZip (Query (MVR a) ([] `Compose` S a)) where
    sunzip Get_MVR = Map.fromSet (const Get_MVR) psRW
    szip ss = Get_MVR

instance SZip (Update (MVR a)) where
    sunzip (Upd_MVR s) = fmap Upd_MVR (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Upd_MVR _) xs = Upd_MVR (szip $ szip_upd xs)
        szip_upd [] = Map.empty
        szip_upd ((p,Upd_MVR v):xs) = Map.insert p v (szip_upd xs)

---------------------------

testMVR :: IO ()
testMVR = run 3 testMVR'

testMVR' :: Client (MVR Int) RW ()
testMVR' = do
    cupdate 1 $ Upd_MVR (SIW 5) 
    cupdate 0 $ Upd_MVR (SIW 3) 
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 Get_MVR >>= liftIO . print
