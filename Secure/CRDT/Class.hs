{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | CRDT type class.
module Secure.CRDT.Class where

import Secure.CRDT.Utils

-- | Abstract interface for CRDTs, supporting monadic operations.
class (Monad m) => CRDT n m where
    -- | The internal state kept by each replica.
    data State n m :: *
    -- | The supported query operations posed by clients to a replica, producing a result of type `r`.
    data Query n (r :: (* -> *) -> *) m :: *
    -- | The supported update operations posed by clients to a replica, altering the replica's internal state.
    data Update n m :: *
    -- | The type of messages internally exchanged among replicas during propagate/merge.
    data Message n m :: *
    
    -- | Creates a new internal state for a replica.
    new :: R -> m (State n m)
    -- | Performs a query operation on a replica.
    query :: R -> Query n r m -> State n m -> m (r m)
    -- | Performs an update operation on a replica.
    update :: R -> Update n m -> State n m -> m (State n m)
    -- | Propagates the state of the first replica into a message send to the second replica.
    propagate :: R -> R -> State n m -> m (Message n m)
    -- | Merges a message from the first replica into the state of the second replica.
    merge :: R -> R -> Message n m -> State n m -> m (State n m)