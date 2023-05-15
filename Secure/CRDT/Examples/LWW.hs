{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure last-writer wins.
module Secure.CRDT.Examples.LWW where

import Secure.CRDT

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Control.Monad
import Control.Monad.IO.Class

data LWW (a :: *)
instance World w => CRDT (LWW a) w where
    data State (LWW a) w = State_LWW (Maybe (S a w,LC))
    data Update (LWW a) w = Upd_LWW (S a w) T
    data Query (LWW a) r w where
        Get_LWW :: Query (LWW a) (Maybe `Compose` S a) w
    data Message (LWW a) w = Message_LWW (Maybe (S a w,LC))
    
    new i = return $ State_LWW Nothing
    query i Get_LWW (State_LWW st) = return $ Compose $ fmap fst st
    update i (Upd_LWW v t) (State_LWW st) = return $ State_LWW $ if Just (LC t i) > fmap snd st then Just (v,LC t i) else st
    propagate i j (State_LWW st_i) = return $ Message_LWW st_i
    merge i j (Message_LWW m_i) (State_LWW st_j) = return $ State_LWW $ if fmap snd m_i > fmap snd st_j then m_i else st_j

instance SZip (Query (LWW a) (Maybe `Compose` S a)) where
    sunzip Get_LWW = Map.fromSet (const Get_LWW) psRW
    szip ss = Get_LWW

instance SZip (Update (LWW a)) where
    sunzip (Upd_LWW s t) = fmap (\s -> Upd_LWW s t) (sunzip s)
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Upd_LWW _ t) xs = Upd_LWW (szip $ szip_upd xs) t
        szip_upd [] = Map.empty
        szip_upd ((p,Upd_LWW v _):xs) = Map.insert p v (szip_upd xs)

---------------------------

testLWW :: IO ()
testLWW = run 3 testLWW'

testLWW' :: Client (LWW Int) RW ()
testLWW' = do
    cupdate 0 $ Upd_LWW (SIW 3) (T 5)
    cupdate 1 $ Upd_LWW (SIW 5) (T 4)
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 Get_LWW >>= liftIO . print

