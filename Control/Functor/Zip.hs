module Control.Functor.Zip where

import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map

class Functor f => Zip f where
    fzip :: f a -> f b -> f (a, b)
    fzip = fzipWith (,)
    fzipWith :: (a -> b -> c) -> f a -> f b -> f c
    fzipWith f as bs = fmap (uncurry f) (fzip as bs)
    
fzipMap :: (Ord k,Zip f) => Map k (f a) -> f (Map k a)
fzipMap m = fzipMap' (Map.toList m)
    where
    fzipMap' [(k,fa)] = fmap (Map.singleton k) fa
    fzipMap' (x:xs) = fzipWith Map.union (fzipMap' [x]) (fzipMap' xs)
    
instance Zip Maybe where
    fzipWith f (Just a) (Just b) = Just (f a b)
    fzipWith _ _ _ = Nothing
    
instance Zip [] where
    fzip = zip
    fzipWith = zipWith

instance Ord k => Zip (Map k) where
    fzipWith f = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched $ \k -> f)
    