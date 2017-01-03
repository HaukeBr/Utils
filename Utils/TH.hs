{-# LANGUAGE TemplateHaskell, CPP #-}
module Utils.TH where
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace

--------------------------------------------------------------------------------

#ifndef NOD
#define NOD 5
#endif

#if defined(NOD) && NOD == 0
#define QNOD reportWarning "NOD == 0, endless loop! Setting it to (-1) to be at least able to compile!\n" >> return (-1)
#else
#define QNOD return (NOD)
#endif

--------------------------------------------------------------------------------

genSingle :: (Int -> Q Exp) -> String -> Int -> Q [Dec]
genSingle maker funname n = return <$> genNth maker funname n

genNth :: (Int -> Q Exp) -> String -> Int -> Q Dec
genNth maker funname n | n > 0 = do
	expr <- maker n
	let name = mkName $ funname ++ show n
	return $ FunD name [Clause [] (NormalB expr) []]

genN :: (Int -> Q Exp) -> String -> Int -> Q [Dec]
genN maker funname n = genNs maker funname [2..n]

genNs :: (Int -> Q Exp) -> String -> [Int] -> Q [Dec]
genNs maker funname ns
	| any (< 0) ns || null ns	 = do
		reportWarning $ "genN called with a negative value, there will be nothing generated with name \"" ++ funname ++ "\""  
		return []
	| any (> 0) ns = forM (reverse $ ns) (genNth maker funname)
	| any (== 0) ns = do
		tnod <- QNOD
		genN maker funname tnod

--------------------------------------------------------------------------------

getType :: Name -> Q (Maybe Type)
getType typName = do
	res <- reify typName
	return $ ConT <$> case res of 
		(TyConI d) -> case d of
			DataD _ typ _ _ _ _ -> Just typ
			NewtypeD _ typ  _ _ _ _ -> Just typ
		_ -> Nothing

genPure :: Q Dec
genPure = do
	ret <- [| return |]
	return $ FunD 'pure [Clause [] (NormalB ret) []]

genFu :: Q Dec
genFu = do
	fmap_ <- [| \f a -> a >>= return . f |]
	return $ FunD 'fmap [Clause [] (NormalB fmap_) []]	

genFuInsts :: [Name] -> Q [Dec]
genFuInsts xs =  concat <$> mapM genFuInst xs

genFuInst :: Name -> Q [Dec]
genFuInst typName = do
	unboxed <- getType typName
	tp <- case unboxed of
		Nothing -> fail "Error getting type in deriving Functor" >> return undefined
		Just typ -> return typ
	fmapDef <- genFu
	return $ return $ InstanceD Nothing [] (AppT (ConT ''Functor) tp) [fmapDef]

genAp :: Q Dec
genAp = do
	ap_ <- [| ap |]
	return $ FunD '(<*>) [Clause [] (NormalB ap_) []]	

genApInsts :: [Name] -> Q [Dec]
genApInsts xs =  concat <$> mapM genApInst xs

genApInst :: Name -> Q [Dec]
genApInst typName = do
	unboxed <- getType typName
	tp <- case unboxed of
		Nothing -> fail "Error getting type in deriving Applicative" >> return undefined
		Just typ -> return typ
	apDef <- genAp
	pureDef <- genPure
	return $ return $ InstanceD Nothing [] (AppT (ConT ''Applicative) tp) [pureDef, apDef]

genInst :: Name -> Q [Dec]
genInst typName = liftM2 (++) (genApInst typName) (genFuInst typName)

genInsts :: [Name] -> Q [Dec]
genInsts xs = concat <$> mapM genInst xs

--------------------------------------------------------------------------------

listArrow :: [Type] -> Type
listArrow [x] = x
listArrow (x:xs) = AppT (AppT ArrowT x) (listArrow xs)

func :: Type -> Type -> Type
func a b = AppT (AppT (ArrowT) a) b

tupleType :: [Type] -> Type
tupleType xs = foldl AppT (TupleT (length xs)) xs

--------------------------------------------------------------------------------

curryN :: Int -> Q Exp
curryN n | n > 0 = do
	f <- newName "f"
	xs <- replicateM n $ newName "x"
	let args = map VarP $ f:xs
	return $ LamE args $ AppE (VarE f) (TupE (map VarE xs))

uncurryN :: Int -> Q Exp
uncurryN n | n > 0 = do
	f <- newName "f"
	xs <- replicateM n $ newName "x"
	let args = [VarP f, TupP $ map VarP $ xs]
	return $ LamE args $ foldl AppE (VarE f) (map VarE xs)

apN :: Int -> Q Exp
apN n | n > 0 = do
	f <- newName "f"
	xs <- replicateM n (newName "x")
	let args = map VarP (last xs:f:init xs)
	let lambda = LamE args $ foldl AppE (VarE f) (map VarE xs)

	x <- VarT <$> newName "x"
	as <- (fmap VarT) <$> replicateM (n-1) (newName "a")
	b <- VarT <$> newName "b"
	let signature = listArrow [x, listArrow (as ++ [x,b]), listArrow (as ++ [b])]

	return $ SigE lambda signature



mkTN :: Int -> Q Exp
mkTN n | n > 0 = do
	x <- newName "x"
	let lambda 	= LamE [VarP x] (TupE $ replicate n (VarE x))
	
	a <- newName "a"
	let tupSig = foldl (AppT) (TupleT n) $ replicate n (VarT a)
	let signature = AppT (AppT ArrowT (VarT a)) tupSig
	
	return $ SigE lambda signature


mkLN :: Int -> Q Exp
mkLN n | n > 0 = do
	xs <- replicateM n (newName "x")
	let lambda = LamE (map VarP xs) (ListE $ map VarE xs)

	a <- newName "a"
	let signature = listArrow $ replicate n (VarT a) ++ [AppT ListT $ VarT a]

	return lambda
	return $ SigE lambda signature


fmapN :: Int -> Q Exp
fmapN n | n > 0 = do
	f <- newName "f"
	xs <- replicateM n $ newName "x"
	let pat = [VarP f, TupP $ map VarP xs]
	let lambda = LamE pat (TupE $ map (AppE (VarE f) . VarE) xs)

	a <- VarT <$> newName "a"
	b <- VarT <$> newName "b"
	let signature = listArrow [func a b, tupleType (replicate n a), tupleType (replicate n b)]

	return $ SigE lambda signature

	

constN :: Int -> Q Exp
constN n | n > 0 = do
	v <- newName "v"
	xs <- replicateM n $ newName "x"
	let pat = map VarP (v:xs)
	let lambda = LamE pat (VarE v)

	x <- VarT <$> newName "x"
	as <- fmap VarT <$> (replicateM n $ newName "a")
	let signature = listArrow (x:as ++ [x])

	return $ SigE lambda signature

--------------------------------------------------------------------------------
