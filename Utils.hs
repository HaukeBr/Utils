{-# LANGUAGE TemplateHaskell #-}
module Utils (module Utils.TH, module Utils.Utils, module Utils.Joinable) where
import Utils.TH
import Utils.Utils
import Utils.Joinable

data Cont a = Cont a
instance Monad Cont where
 	(Cont v) >>= f = f v
	return = Cont

$(genInst ''Cont)
