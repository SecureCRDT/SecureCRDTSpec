{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, ScopedTypeVariables, FlexibleContexts, TypeOperators #-}

-- | Interface for animating the interaction of CRDT clients with multiple distributed secure replicas.
module Secure.CRDT.Client
  ( type Client, run, cquery, cupdate, cpropagate, cmerge
  ) where

import Secure.CRDT.Utils
import Secure.CRDT.Class
import Secure.CRDT.IO
import Secure.CRDT.World
import Secure.CRDT.World.Ideal
import Secure.CRDT.World.Real

import Data.Typeable
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe
import qualified Control.Monad.Reader as M
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.Async

-- | The global state, which keeps information about CRDT replicas and MPC parties.
type Client n w = M.ReaderT (Replicas n w) IO
type Replicas n w = Map R (Replica n w)
type Replica n w = Map P (Party n w)
-- | The state of a MPC party.
data Party n w = Party
    { inBuffer :: Chan (Input n w) -- ^ an input buffer to receive requests from the client.
    , outBuffer :: Chan (Output n w) -- ^ an output buffer to send responses to the client.
    , state :: MVar (State n w) -- ^ the internal CRDT state kept by the the party.
    , fromChans :: Map R (Chan (Message n w)) -- ^ an incoming channel to receive messages from the same party in other replicas.
    , toChans :: Map R (Chan (Message n w)) -- ^ an outgoing channel to send messages to the same party in other replicas.
    }

-- | Sends a client input to a given replica.
setInput :: SIO n w => R -> Input n IW -> Client n w ()
setInput r i = do
    let is = sinput i
    replicas <- M.ask
    let replica = mlookup r replicas
    M.lift $ forM_ (mzip replica is) $ \(party,ip) -> do
        writeChan (inBuffer party) ip

-- | Receives a client output from a given replica.
getOutput :: SIO n w => R -> Client n w (Output n IW)
getOutput r = do
    replicas <- M.ask
    let replica = mlookup r replicas
    os <- M.lift $ forM replica $ \party -> do
        readChan (outBuffer party)
    return $ soutput os

-- | simulates a distributed CRDT with a given number of replicas.
-- If in the ideal world, with a single idealized MPC party.
-- If in the real world, with a fixed number of 3 dummy MPC parties.
run :: (World w,CRDT n w) => R -> Client n w () -> IO ()
run r (c::Client n w ()) = do
    let w = Proxy :: Proxy w
    let rs = Set.fromList [0..r-1]
    chans <- fromSetM (const $ fromSetM (const $ fromSetM (const newChan) rs) rs) (ps w)
    replicas <- flip fromSetM rs $ \r -> do
        flip fromSetM (ps w) $ \p -> do
            inchan <- newChan
            writeChan inchan New
            outchan <- newChan
            state <- newEmptyMVar
            let fromchans = fmap (mlookup r) (mlookup p chans)
            let tochans = mlookup r (mlookup p chans)
            return $ Party inchan outchan state fromchans tochans
    wrefs <- fromSetM (const $ initWorld w) rs
    -- launches a thread that processes client requests and run each party (of each replica) in parallel in a dedicated thread.
    race_ (M.runReaderT c replicas) $ forConcurrently_ rs $ \r -> forConcurrently_ (ps w) $ \p -> do
        let wref = mlookup r wrefs
        let party = mlookup p (mlookup r replicas)
        -- each party runs an infinite loop that reads inputs with a request, runs a MPC protocol and returns an optional output
        forever $ do
            i <- readChan (inBuffer party)
            case i of
                New -> do
                    runWorld wref p (new r) >>= putMVar (state party)
                Query q -> do
                    readMVar (state party) >>= runWorld wref p . query r q >>= writeChan (outBuffer party) . Out q
                Update u -> do
                    readMVar (state party) >>= runWorld wref p . update r u >>= writeMVar (state party)
                Propagate rto -> do
                    msg <- readMVar (state party) >>= runWorld wref p . propagate r rto
                    writeChan (mlookup rto $ toChans party) msg
                Merge rfrom -> do
                    msg <- readChan (mlookup rfrom $ fromChans party)
                    readMVar (state party) >>= runWorld wref p . merge rfrom r msg >>= writeMVar (state party)

-- | Client interface for performing a query on a given replica, and wait for the query result.
cquery :: (SZip (Query n r),SZip r,SIO n w,Typeable r) => R -> Query n r IW -> Client n w (r IW)
cquery r q = do
    setInput r (Query q)
    Out _ res <- getOutput r
    return $ maybe (error "cquery") id $ fcast Proxy res
    
-- | Client interface for performing an update on a given replica.
cupdate :: (SZip (Update n),SIO n w) => R -> Update n IW -> Client n w ()
cupdate r u = setInput r (Update u)

-- | Client interface for propagating the state from a first to a second replica.
cpropagate :: SIO n w => R -> R -> Client n w ()
cpropagate i j = setInput i $ Propagate j

-- | Client interface for merging a message from a first replica into the state of a second replica. 
cmerge :: SIO n w => R -> R -> Client n w ()
cmerge i j = setInput j $ Merge i

