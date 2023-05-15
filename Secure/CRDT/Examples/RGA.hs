{-# LANGUAGE TypeFamilies, GADTs, TypeOperators, TupleSections, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

-- | Secure replicable growable array.
module Secure.CRDT.Examples.RGA where

import Secure.CRDT

import Data.Tuple.Utils
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Product
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class

data RGA (a :: *)
instance World w => CRDT (RGA a) w where
    data State (RGA a) w = State_RGA ([(LC,S a w,Maybe LC)],MC,Maybe (Update (RGA a) w))
    data Update (RGA a) w = Ins_RGA (Maybe LC) (S a w) | Del_RGA LC
    data Query (RGA a) r w where
        Get_RGA :: Query (RGA a) ([] `Compose` ((Const LC) `Product` S a)) w
    data Message (RGA a) w = Message_RGA (Maybe (VC,Update (RGA a) w))
    
    new i = return $ State_RGA $ ([],Map.empty,Nothing)
    query i Get_RGA (State_RGA (xs,_,_)) = do
        return $ Compose $ foldr (\(c,v,b) ys -> if isJust b then ys else (Pair (Const c) v):ys) [] xs
    update i u st = return $ update_RGA' Nothing i u st
    propagate i j (State_RGA (st,mc,u)) = return $ Message_RGA $ fmap (lookupMC i mc,) u
    merge i j (Message_RGA Nothing) st_j = return st_j
    merge i j (Message_RGA (Just (vc_i,u_i))) (State_RGA (xs_j,mc_j,u_j)) = do
        let mc_j' = mergeMC i vc_i mc_j
        return $ update_RGA' (Just (LC (lookupVC i vc_i) i)) i u_i (State_RGA (xs_j,mc_j',u_j))

update_RGA' :: Maybe LC -> R -> Update (RGA a) w -> State (RGA a) w -> State (RGA a) w
update_RGA' mbuid i u (State_RGA st@(xs,mc,_)) = State_RGA $ maybe st (,mc',Just u) $ apply_RGA mc uid u xs
    where
    vc = lookupMC i mc
    vc' = incVC i vc
    mc' = Map.insert i vc' mc
    uid = maybe (LC (lookupVC i vc') i) id mbuid

apply_RGA :: MC -> LC -> Update (RGA a) w -> [(LC,S a w,Maybe LC)] -> Maybe [(LC,S a w,Maybe LC)]
apply_RGA mc c (Ins_RGA j e) xs = insert_RGA mc j (c,e) xs
apply_RGA mc c (Del_RGA j) xs = delete_RGA mc c j xs

insert_RGA :: MC -> Maybe LC -> (LC,S a w) -> [(LC,S a w,Maybe LC)] -> Maybe [(LC,S a w,Maybe LC)]
insert_RGA mc Nothing e xs = Just $ insertBody mc e xs
insert_RGA mc (Just j) e [] = Nothing
insert_RGA mc (Just j) e (x@(i_x,v_x,b_x):xs) = if j == i_x
    then Just (purgeCons mc x $ insertBody mc e xs)
    else fmap (purgeCons mc x) (insert_RGA mc (Just j) e xs)

insertBody :: MC -> (LC,S a w) -> [(LC,S a w,Maybe LC)] -> [(LC,S a w,Maybe LC)]
insertBody mc (i,v) [] = [(i,v,Nothing)]
insertBody mc e@(i,v) (x:xs) = if i > fst3 x
    then (i,v,Nothing) : purgeCons mc x xs
    else purgeCons mc x (insertBody mc e xs)

delete_RGA :: MC -> LC -> LC -> [(LC,S a w,Maybe LC)] -> Maybe [(LC,S a w,Maybe LC)]
delete_RGA mc k j [] = Nothing
delete_RGA mc k j (x@(i,v,b):xs) = if i == j
    then Just ((i,v,Just k) : xs)
    else fmap (purgeCons mc x) (delete_RGA mc k j xs)
    
purgeCons :: MC -> (LC,S a w,Maybe LC) -> [(LC,S a w,Maybe LC)] -> [(LC,S a w,Maybe LC)]
purgeCons mc (i,v,Just k) xs | leMC k mc = xs
purgeCons mc x xs = x : xs

instance SZip (Query (RGA a) ([] `Compose` ((Const LC) `Product` S a))) where
    sunzip Get_RGA = Map.fromSet (const Get_RGA) psRW
    szip ss = Get_RGA

instance SZip (Update (RGA a)) where
    sunzip (Ins_RGA p s) = fmap (Ins_RGA p) (sunzip s)
    sunzip (Del_RGA p) = Map.fromSet (const $ Del_RGA p) psRW
    szip ss = szip' (mlookup 0 ss) (Map.toList ss)
        where
        szip' (Ins_RGA l _) xs = Ins_RGA l $ szip $ szip_ins xs
        szip' (Del_RGA l) xs = Del_RGA l
        szip_ins [] = Map.empty
        szip_ins ((p,Ins_RGA l v):xs) = Map.insert p v (szip_ins xs)
        szip_ins (x:xs) = szip_ins xs

---------------------------

testRGA :: IO ()
testRGA = run 3 testRGA'

testRGA' :: Client (RGA Int) RW ()
testRGA' = do
    return ()
    cupdate 0 $ Ins_RGA Nothing (SIW 3)
    Compose [Pair (Const p3) _] <- cquery 0 Get_RGA
    cpropagate 0 1 >> cmerge 0 1
    cupdate 1 $ Ins_RGA (Just p3) (SIW 4)
    cquery 1 Get_RGA >>= liftIO . print
    cupdate 1 $ Del_RGA p3
    cquery 1 Get_RGA >>= liftIO . print


