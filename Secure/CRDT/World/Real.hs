{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

-- | Dummy real world.
-- NOTE: This module only exists to allow simple simulation, and offers no actual security!
module Secure.CRDT.World.Real where

import Secure.CRDT.Utils
import Secure.CRDT.World
import Secure.CRDT.MPC

import Data.List
import Data.Proxy
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as M
import Control.Concurrent.Chan
import System.Random.Shuffle

-- | Monad for the real world where each party keeps a share of the secret value.
type RW = M.ReaderT PW IO
-- | State of a party in the real world
data PW = PW
    { pid :: P -- ^ party id
    , tochans :: Map P DynChan -- ^ channels to other parties in the same replica
    , fromchans :: Map P DynChan -- ^ channels from other parties in the same replica
    }
type DynChan = Chan Dynamic

-- | Send a message to another party in the same replica.
send :: Typeable a => P -> a -> RW ()
send p a = do
    c <- M.asks (mlookup p . tochans)
    liftIO $ writeChan c $ toDyn a
-- | Receive a message from another party in the same replica.
recv :: Typeable a => P -> RW a
recv p = do
    c <- M.asks (mlookup p . fromchans)
    liftM (maybe (error "recv") id . fromDynamic) $ liftIO $ readChan c
-- | The next party (for a fixed number of 3 parties).
nextP :: P -> P
nextP p = (p+1) `mod` 3
-- | The previous party (for a fixed number of 3 parties).
prevP :: P -> P
prevP p = (p-1) `mod` 3
-- | The current party.
getP :: RW P
getP = M.asks pid

-- | Real world instance.
-- We create 3 dummy MPC parties per replica.
instance World RW where
    ps _ = Set.fromList [0,1,2]
    type WorldRef RW = Map P PW
    initWorld w = do
        chans <- fromSetM (const $ fromSetM (const $ newChan) (ps w)) (ps w)
        return $ Map.fromSet (\p -> PW p (mlookup p chans) (fmap (mlookup p) chans)) (ps w)
    runWorld wr p w = M.runReaderT w (mlookup p wr)
    
psRW = ps (Proxy::Proxy RW)

-- | Dummy MPC instance for the real world.
-- NOTE: This is blatantly insecure: each party keeps a copy of the secret value!
instance MPC RW a where
    data S a RW = SRW a deriving Show
    classify a = return (SRW a)
    declassify (SRW a) = return a
    
-- | Dummy numerical MPC instance for the real world.
-- We implement a dummy MPC protocol: we have each party send its shares to the next party and computes the result over the received shares of the previous party.
-- NOTE: This is blatantly insecure: each party keeps a copy of the secret value!
instance (Typeable a,Num a) => SNum RW a where
    splus (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        i <- recv (prevP p)
        j <- recv (prevP p)
        return $ SRW $ i + j
    sminus (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        i <- recv (prevP p)
        j <- recv (prevP p)
        return $ SRW $ i - j
    sif (SRW b) x y = do
        p <- getP
        send (nextP p) b
        b <- recv (prevP p)
        return $ if b then x else y
        
-- | Dummy ordering MPC instance for the real world.
-- We implement a dummy MPC protocol: we have each party send its shares to the next party and computes the result over the received shares of the previous party.
-- NOTE: This is blatantly insecure: each party keeps a copy of the secret value!
instance (Typeable a,Ord a) => SOrd RW a where
    sge (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        (i::a) <- recv (prevP p)
        (j::a) <- recv (prevP p)
        return $ SRW $ i >= j
    smax (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        i <- recv (prevP p)
        j <- recv (prevP p)
        return $ SRW $ max i j

-- | Dummy equality MPC instance for the real world.
-- We implement a dummy MPC protocol: we have each party send its shares to the next party and computes the result over the received shares of the previous party.
-- NOTE: This is blatantly insecure: each party keeps a copy of the secret value!
instance (Typeable a,Eq a) => SEq RW a where
    seq (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        (i::a) <- recv (prevP p)
        (j::a) <- recv (prevP p)
        return $ SRW $ i == j

-- | Dummy bool MPC instance for the real world.
-- We implement a dummy MPC protocol: we have each party send its shares to the next party and computes the result over the received shares of the previous party.
-- NOTE: This is blatantly insecure: each party keeps a copy of the secret value!
instance SBool RW where
    sor (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        (i) <- recv (prevP p)
        (j) <- recv (prevP p)
        return $ SRW $ i || j
    sand (SRW i) (SRW j) = do
        p <- getP
        send (nextP p) i
        send (nextP p) j
        (i) <- recv (prevP p)
        (j) <- recv (prevP p)
        return $ SRW $ i && j
    snot (SRW i) = do
        p <- getP
        send (nextP p) i
        (i) <- recv (prevP p)
        return $ SRW $ not i

instance SShuffle RW a where
    sshuffle xs = do
        p <- getP
        permP <- liftIO $ mkPerm xs
        send (prevP p) permP
        send (nextP p) permP
        permPrev <- recv (prevP p)
        permNext <- recv (nextP p)
        let perms = map snd $ sort [(p,permP),(prevP p,permPrev),(nextP p,permNext)]
        return $ applyPerms perms xs

mkPerm :: [a] -> IO [Int]
mkPerm xs = liftM tail $ shuffleM [0..length xs - 1]

applyPerms :: [[Int]] -> [a] -> [a]
applyPerms [] xs = xs
applyPerms (p:ps) xs = applyPerms ps (shuffle xs p)

