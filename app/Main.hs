module Main where

-- The kind of f is * -> * like Maybe, [].
-- The kind of a is * like Int, Char, [Char].
data Free f a = Pure a | Free (f (Free f a)) 

-- functor is typeclass which "fmap" is defined
-- (<$>) :: Functor f => (a->b) -> f a -> f b
-- (<$>) = fmap
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

-- applicative is typeclass which derived from functor
-- and offer "pure" and <*>
-- (<*>) :: f (a -> b) -> f a -> f b
instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
    return = Pure
    Pure a >>= k = k a
    Free fm >>= k = Free (fmap (>>= k) fm)

-- free1 :: Num a => Free f a
free1 :: Free [] Int
free1 = Pure 10

free2 :: Free [] Int
free2 = Free [Free [Free [Free []]]]

free3 :: Free [] Int
free3 = Free [Pure 10]

data Game g = GetInput (String -> g) | Render String g

main :: IO ()
main = putStrLn "hello"
