{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, FlexibleContexts, UndecidableInstances, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure maximum.
module Secure.CRDT.Examples.MAX where

import Secure.CRDT

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class

data MAX
instance (World w,SNum w Int,SOrd w Int) => CRDT MAX w where
    data State MAX w = State_MAX (S Int w)
    data Update MAX w = Set_MAX { set_MAX :: S Int w }
    data Query MAX r w where
        Get_MAX :: Query MAX (S Int) w
    data Message MAX w = Message_MAX (S Int w)
    
    new i = liftM State_MAX $ classify minBound
    query i Get_MAX (State_MAX st) = return st
    update i (Set_MAX n) (State_MAX st) = liftM State_MAX $ smax n st
    propagate i j (State_MAX st_i) = return $ Message_MAX st_i
    merge i j (Message_MAX m_i) (State_MAX st_j) = liftM State_MAX $ smax m_i st_j

instance SZip (Query MAX (S Int)) where
    sunzip Get_MAX = Map.fromSet (const Get_MAX) psRW
    szip ss = Get_MAX

instance SZip (Update MAX) where
    sunzip (Set_MAX s) = fmap Set_MAX (sunzip s)
    szip ss = Set_MAX $ szip (Map.map set_MAX ss)

---------------------------

testMAX :: IO ()
testMAX = run 3 testMAX'

testMAX' :: Client MAX RW ()
testMAX' = do
    cupdate 1 $ Set_MAX (SIW 0)
    cquery 1 Get_MAX >>= liftIO . print
    cupdate 0 $ Set_MAX (SIW 3)
    cpropagate 0 1 >> cmerge 0 1
    cquery 1 Get_MAX >>= liftIO . print