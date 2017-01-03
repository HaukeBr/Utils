{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Utils.Joinable(Joinable(..)) where

class Joinable j where
	jn :: j (j a) -> j a

instance Monad m => Joinable m where
	jn = (>>= id) 

-- TODO
{- Soll dann automatisch geschehen, er soll nach dieser Def suchen und die
 - Monad instanz -- falls notwendig -- automatisch erzeugen -}
--instance (Applicative m, Joinable m) => Monad m where
--	(>>=) = flip (((.).(.)) jn (<*>) . return)
-- das könnte mit reifyInstances gehen
-- sonst muss man das auf die unelegante Weiße wie 
-- QuickCheck machen und die Datei öffnen, selbst
-- Pi mal Daumen (überapporx.) lexen und dann mir reify o.ä.
-- immer gegen checken, ob das gefundene Ding in der gesuchten
-- Form definiert ist oder nicht (quasi BruteForce reify mit
-- Tokens aus der Datei als Dictionary)
