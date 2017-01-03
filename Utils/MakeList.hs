{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Utils.MakeList (mkL, RetClass()) where

class RetClass a r | r -> a where
	proc :: a -> [a] -> r

instance RetClass a [a] where
	proc = (:)

instance RetClass a r => RetClass a (a -> r)  where
	proc e acc = \a -> proc e (a:acc)

-- | Polyvariardic function, generate a 
--   list consisting of all arguments.
mkL :: RetClass a r => a -> r
mkL e = proc e []
