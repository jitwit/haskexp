{-# language GADTs, LambdaCase, MultiWayIf, TypeApplications #-}

module Language.HExp where

import Data.String
import Data.Word
import Data.List
import Data.Foldable
import Data.Vector (Vector)
import Data.Map (Map)
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Complex
import Data.Tree
import GHC.Real

put_hexp :: SEXP -> IO ()
put_hexp = putStrLn . show

he1 :: SEXP
he1 = HExpList [HExpInteger 1, HExpInteger 2]
he2 :: SEXP
he2 = HExpChar 'x'
he3 :: SEXP
he3 = HExpList [HExpChar 'x', HExpChar 'y']
he4 :: SEXP
he4 = HExpList [ HExpSymbol "cat"
               , HExpList [HExpString "bird", HExpString "wolf"]]
he5 = HExpCons he2 he3
he6 = HExpCons (HExpString "cat") (HExpInteger 12)
he7 = HExpRatio (1 :% 3)
he8 = HExpComplex (1 :+ 3)
he9 = HExpComplex (1 :+ (-3))
he10 = HExpComplex (1 :+ 0)

hv1 = sexp_of @(Int,Integer) (12,13)
hv2 = sexp_of [1..10 :: Int]
hv3 = HExpQuote hv1
hv4 = HExpQuasiQuote hv1
hv5 = sexp_of ()
hv6 = sexp_of ((), (1 :+ 2) :: Complex Double, (1 :% 123) :: Rational)
t1,t2 :: Tree Int
t1 = Node 1 []
t2 = Node 3 [Node 4 [Node 6 [], Node 8 []], Node 5 []]
hv7 = sexp_of t1
hv8 = sexp_of t2
hv9 = HExpCons hv2 he4
hv10 = HExpCons hv2 he7
hv11 = HExpCons hv2 (HExpList [he7])

hexps1 = [he1,he2,he3,he4,he5,he6,he7,he8,he9,he10
         ,hv1,hv2,hv3,hv4,hv5,hv6,hv7,hv8,hv9,hv10,hv11]

ghcid = do traverse_ put_hexp hexps1

data SEXP where
  HExpBool :: Bool -> SEXP
  HExpInteger :: Integer -> SEXP
  HExpDouble :: Double -> SEXP
  HExpFloat :: Float -> SEXP
  HExpComplex :: (Ord h, Num h, Show h) => Complex h -> SEXP
  HExpRatio :: Show h => Ratio h -> SEXP
  HExpChar :: Char -> SEXP
  HExpString :: String -> SEXP
  HExpSymbol :: String -> SEXP
  HExpList :: [SEXP] -> SEXP
  HExpCons :: SEXP -> SEXP -> SEXP
  HExpVector :: Vector SEXP -> SEXP
  HExpQuote :: SEXP -> SEXP
  HExpQuasiQuote :: SEXP -> SEXP

instance Show SEXP where
  show = \case
    HExpList xs -> '(' : (unwords $ show <$> xs) <> ")"
    HExpBool b -> if b then "#t" else "#f"
    HExpInteger x -> show x
    HExpChar x -> "#\\" <> [x]
    HExpString x -> show x
    HExpSymbol x -> x
    HExpDouble x -> show x
    HExpFloat x -> show x
    HExpRatio (x :% y) -> concat [show x,"/",show y]
    HExpComplex (x :+ y) ->
      if | y > 0 -> concat [show x,"+",show y,"i"]
         | y < 0 -> concat [show x,"-",show (- y),"i"]
         | otherwise -> show x
    HExpCons x y -> case y of
      HExpList ys -> show (HExpList (x:ys))
      _ -> '(' : unwords [show x, ".", show y]  <> ")"
    HExpVector v -> '#' : show (HExpList (toList v)) 
    HExpQuote x -> '\'' : show x
    HExpQuasiQuote x -> '`' : show x

class SEXPOf h where
  sexp_of :: h -> SEXP

instance SEXPOf () where
  sexp_of () = HExpList []

instance SEXPOf Bool where
  sexp_of = HExpBool

instance SEXPOf Char where
  sexp_of = HExpChar

instance SEXPOf Int where
  sexp_of = HExpInteger . fromIntegral

instance SEXPOf Word where
  sexp_of = HExpInteger . fromIntegral

instance SEXPOf Integer where
  sexp_of = HExpInteger

instance SEXPOf Double where
  sexp_of = HExpDouble
  
instance (Show v, Num v, Ord v, SEXPOf v) => SEXPOf (Complex v) where
  sexp_of = HExpComplex

instance (Show v, SEXPOf v) => SEXPOf (Ratio v) where
  sexp_of = HExpRatio

instance SEXPOf h => SEXPOf [h] where
  sexp_of = HExpList . map sexp_of

instance (SEXPOf a, SEXPOf b) => SEXPOf (a,b) where
  sexp_of (x,y) = HExpCons (sexp_of x) (sexp_of y)

instance (SEXPOf a, SEXPOf b, SEXPOf c) => SEXPOf (a,b,c) where
  sexp_of (x,y,z) = HExpList [sexp_of x, sexp_of y, sexp_of z]

instance (SEXPOf a, SEXPOf b, SEXPOf c, SEXPOf d)
      => SEXPOf (a,b,c,d) where
  sexp_of (x,y,z,w) = HExpList [sexp_of x, sexp_of y, sexp_of z, sexp_of w]

instance (SEXPOf k, SEXPOf v) => SEXPOf (Map k v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (IntMap v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (Set v) where
  sexp_of = sexp_of . toList

instance SEXPOf h => SEXPOf (Vector h) where
  sexp_of = HExpVector . fmap sexp_of

instance SEXPOf h => SEXPOf (Tree h) where
  sexp_of (Node x []) = sexp_of x
  sexp_of (Node x xs) = HExpCons (sexp_of x) (sexp_of xs)

instance Semigroup SEXP where
  HExpList xs <> HExpList ys = HExpList (xs <> ys)
  HExpList xs <> y = if null xs then y else HExpList (xs <> [y])
  x <> HExpList ys = if null ys then x else HExpList (x:ys)
  x <> y = HExpCons x y

instance Monoid SEXP where
  mempty = HExpList []

  
--  a@Atom{} <> b@Atom{} = List [a,b]
--  a@Atom{} <> List xs = List (a:xs)
--  List xs <> b@Atom{} = List (xs <> [b])
--  List xs <> List ys = List (xs <> ys)

--instance Monoid (SEXP a) where
--  mempty = List []
    
