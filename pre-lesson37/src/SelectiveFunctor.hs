{-# LANGUAGE TypeOperators, PatternSynonyms, ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module SelectiveFunctor where

--https://www.reddit.com/r/haskell/comments/axje88/selective_applicative_functors/
--https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf
--https://gist.github.com/LSLeary/c6a43e42bb08c27ef1debf4cc5f5b1a0

newtype a -? b = Lazy { unLazy :: Either (a -> b) b }
infixr 0 -?

class Applicative f => Selective f where
  {-# MINIMAL (<*?) | liftS2 #-}

  (<*?) :: f (a -? b) -> f a -> f b
  (<*?) = undefined --liftS2 id
  infixl 4 <*?

  liftS2 :: (a -? b -? c) -> f a -> f b -> f c
  liftS2 g fa fb = pure g <*? fa <*? fb



--whenS :: IO Bool -> IO () -> IO ()
whenS b a = selectM b a

selectM :: Monad f => f (Either a b) -> f (a -> b) -> f b
selectM x y = x >>= \e ->
    case e of
        Left a -> ($a) <$> y -- Execute y
        Right b -> pure b    -- Skip y

--pingPongS = whenS (fmap (=="ping") getLine) (putStrLn "pong")


selectA :: Applicative f => f (Either a b) -> f (a -> b) -> f b
selectA x y = (\e f -> either f id e) <$> x <*> y -- Execute x and y



