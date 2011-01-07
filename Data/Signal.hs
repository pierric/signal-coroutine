{-# LANGUAGE PatternGuards #-}
module Signal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Applicative

newtype Monad m => Signal m a = Signal {unSignal :: Coroutine (Yield a) m ()}

instance Monad m => Functor (Signal m) where
    fmap f = Signal . bounce1 (yield . f) (fmap f) . unSignal
      where bounce1 yield continue c = lift (resume c) >>= connect
              where connect s | Left (Yield a cont) <- s = yield a >> (unSignal . continue . Signal) cont
                              | otherwise = return ()


instance Monad m => Applicative (Signal m) where
    pure = unitS
    u <*> v = Signal $ do f <- lift $ resume $ unSignal u
                          x <- lift $ resume $ unSignal v
                          apply f x
      where apply f x | Left (Yield a contA) <- f
                      , Left (Yield b contB) <- x = yield (a b) >> (unSignal (Signal contA <*> Signal contB))
                      | otherwise = return ()

(+>) :: Monad m => a -> Signal m a -> Signal m a
(+>) x xs = Signal $ yield x >> unSignal xs

(<+) :: Monad m => Signal m a -> a -> Signal m a
(<+) xs x = Signal $ unSignal xs >> yield x

headS :: Monad m => Signal m a -> m a
headS sig  = resume (unSignal sig) >>= either (\(Yield elem _) -> return elem) (error "empty signal")

tailS :: Monad m => Signal m a -> m (Signal m a)
tailS sig  = resume (unSignal sig) >>= either (\(Yield _ next) -> return (Signal next)) (error "empty signal")

takeS :: (Integral i, Monad m) => i -> Signal m a -> m [a]
takeS cnt sig = collect (unSignal sig) 0 []
  where collect sig n col | n < cnt   = do s <- resume sig 
                                           case s of
                                             Right _ -> return $ reverse col
                                             Left (Yield s cont) -> collect cont (n+1) (s:col)
                          | otherwise = return $ reverse col

unitS :: Monad m => a -> Signal m a
unitS unit = Signal $ forever $ yield unit

delayS :: Monad m => a -> Signal m a -> Signal m a
delayS = (+>)

infinitS :: Monad m => (a -> a) -> a -> Signal m a
infinitS f a = Signal $ yield a >> unSignal (infinitS f (f a))

mapS :: Monad m => (a -> b) -> Signal m a -> Signal m b
mapS = fmap

zipWithS :: Monad m => (a -> b -> c) -> Signal m a -> Signal m b -> Signal m c
zipWithS f x y = pure f <*> x <*> y

zipWith3S :: Monad m => (a -> b -> c -> d) -> Signal m a -> Signal m b -> Signal m c -> Signal m d
zipWith3S f x y z = pure f <*> x <*> y <*> z

zipWith4S :: Monad m => (a -> b -> c -> d -> e) -> Signal m a -> Signal m b -> Signal m c -> Signal m d -> Signal m e
zipWith4S f x y z u = pure f <*> x <*> y <*> z <*> u

zipWith5S :: Monad m => (a -> b -> c -> d -> e -> f) -> Signal m a -> Signal m b -> Signal m c -> Signal m d -> Signal m e -> Signal m f
zipWith5S f x y z u v = pure f <*> x <*> y <*> z <*> u <*> v

nat :: Signal IO Int
nat = let s = mapS (+1) t
          t = delayS 0 s
      in t

-- what about using Generator?
-- unfoldM :: (b -> IO (Maybe (a, b))) -> b -> m [a]
-- unfoldM gen source = do next <- gen source
                        -- case next of
                          -- Nothing -> return []
                          -- Just (a, source') -> gen source' >>= return . (a:)
