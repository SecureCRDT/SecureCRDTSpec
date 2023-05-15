{-# LANGUAGE TypeFamilies #-}

-- | Abstract world interface.
module Secure.CRDT.World where

import Secure.CRDT.Utils

import Data.Proxy
import Data.Set (Set(..))
import qualified Data.Set as Set

-- | World interface of a replica.
class Monad w => World w where
    -- | returns the fixed number of MPC parties (per replica) in this world.
    ps :: Proxy w -> Set P
    
    -- | An handler to the world, to be used if each party needs to keep some internal state to execute MPC protocols. 
    type WorldRef w :: *
    -- | initializes the world (all parties for a replica).
    initWorld :: Proxy w -> IO (WorldRef w)
    -- | runs a computation for a given party in the world
    runWorld :: WorldRef w -> P -> w a -> IO a