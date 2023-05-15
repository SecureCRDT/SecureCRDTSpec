{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}

-- | MPC library
module Secure.CRDT.MPC where
    
import Secure.CRDT.World

-- | MPC library.
class World w => MPC w a where
    -- | The special type for secret data.
    data S a w :: *
    -- | convert a public value into a secret value.
    classify :: a -> w (S a w)
    -- | convert a secret value into a public value.
    declassify :: S a w -> w a

-- | Numerical class over secret data.
class MPC w a => SNum w a where
    splus :: S a w -> S a w -> w (S a w)
    sminus :: S a w -> S a w -> w (S a w)
    sif :: S Bool w -> S a w -> S a w -> w (S a w)
  
-- | Ordering class over secret data.  
class MPC w a => SOrd w a where
    sge :: S a w -> S a w -> w (S Bool w)
    smax :: S a w -> S a w -> w (S a w)
    
class MPC w a => SEq w a where
    seq :: S a w -> S a w -> w (S Bool w)
    
class MPC w Bool => SBool w where
    sor :: S Bool w -> S Bool w -> w (S Bool w)
    sand :: S Bool w -> S Bool w -> w (S Bool w)
    snot :: S Bool w -> w (S Bool w)
    
class MPC w a => SShuffle w a where
    sshuffle :: [S a w] -> w [S a w]
    

