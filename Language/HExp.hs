{-# language GADTs, LambdaCase, MultiWayIf, TypeApplications #-}

module Language.HExp where

import Data.String
import Data.Word
import Data.List
import Data.Foldable
import Data.Vector (Vector,fromList)
import Data.Array (Array,listArray,bounds,elems)
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
he1 = SList [SInteger 1, SInteger 2]
he2 :: SEXP
he2 = SChar 'x'
he3 :: SEXP
he3 = SList [SChar 'x', SChar 'y']
he4 :: SEXP
he4 = SList [ SSymbol "cat"
            , SList [SString "bird", SString "wolf"]]
he5 = SCons he2 he3
he6 = SCons (SString "cat") (SInteger 12)
he7 = SRatio (1 :% 3)
he8 = SComplex (1 :+ 3)
he9 = SComplex (1 :+ (-3))
he10 = SComplex (1 :+ 0)
he11 = sexp_of (fromList [1,2,3] :: Vector Float)

hv1 = sexp_of @(Int,Integer) (12,13)
hv2 = sexp_of [1..10 :: Int]
hv3 = SQuote hv1
hv4 = SQuasiQuote hv1
hv5 = sexp_of ()
hv6 = sexp_of ((), (1 :+ 2) :: Complex Double, (1 :% 123) :: Rational)
t1,t2 :: Tree Int
t1 = Node 1 []
t2 = Node 3 [ Node 4 [Node 6 [], Node 8 []]
            , Node 5 []
            , Node 8 [ Node 6 []
                     , Node 4 [Node 3
                               [ Node 4 [ Node 6 [], Node 8 []]
                               , Node 5 []
                               , Node 8 [Node 6 [], Node 4 []]]]]]
hv7 = sexp_of t1
hv8 = sexp_of t2
hv9 = SCons hv2 he4
hv10 = SCons hv2 he7
hv11 = SCons hv2 (SList [he7])
a1 :: Array Int Int
a1 = listArray (0,2) [1..3]
ha1 = sexp_of a1
ha2 = SUnquote hv10
ha3 = SList [SSymbol "list", hv3, SUnquoteSplicing hv4]

hexps1 = [he1,he2,he3,he4,he5,he6,he7,he8,he9,he10,he11
         ,hv1,hv2,hv3,hv4,hv5,hv6,hv7,hv8,hv9,hv10,hv11
         ,ha1,ha2,ha3]

ghcid = do traverse_ put_hexp hexps1

data SEXP where
  SBool :: Bool -> SEXP
  SInteger :: Integer -> SEXP
  SDouble :: Double -> SEXP
  SFloat :: Float -> SEXP
  SComplex :: (Ord h, Num h, Show h) => Complex h -> SEXP
  SRatio :: Show h => Ratio h -> SEXP
  SChar :: Char -> SEXP
  SString :: String -> SEXP
  SSymbol :: String -> SEXP
  SList :: [SEXP] -> SEXP
  SCons :: SEXP -> SEXP -> SEXP
  SVector :: Vector SEXP -> SEXP
  SQuote :: SEXP -> SEXP
  SQuasiQuote :: SEXP -> SEXP
  SUnquote :: SEXP -> SEXP
  SUnquoteSplicing :: SEXP -> SEXP

instance Show SEXP where
  show = \case
    SList xs -> '(' : (unwords $ show <$> xs) <> ")"
    SBool b -> if b then "#t" else "#f"
    SInteger x -> show x
    SChar x -> "#\\" <> [x]
    SString x -> show x
    SSymbol x -> x
    SDouble x -> show x
    SFloat x -> show x
    SRatio (x :% y) -> concat [show x,"/",show y]
    SComplex (x :+ y) ->
      if | y > 0 -> concat [show x,"+",show y,"i"]
         | y < 0 -> concat [show x,"-",show (- y),"i"]
         | otherwise -> show x
    SCons x y -> case y of
      SList ys -> show (SList (x:ys))
      _ -> '(' : unwords [show x, ".", show y]  <> ")"
    SVector v -> '#' : show (SList (toList v)) 
    SQuote x -> '\'' : show x
    SQuasiQuote x -> '`' : show x
    SUnquote x -> ',' : show x
    SUnquoteSplicing x -> ",@" <> show x

class SEXPOf h where
  sexp_of :: h -> SEXP

instance SEXPOf () where
  sexp_of () = SList []

instance SEXPOf Bool where
  sexp_of = SBool

instance SEXPOf Char where
  sexp_of = SChar

instance SEXPOf Int where
  sexp_of = SInteger . fromIntegral

instance SEXPOf Word where
  sexp_of = SInteger . fromIntegral

instance SEXPOf Integer where
  sexp_of = SInteger

instance SEXPOf Double where
  sexp_of = SDouble
  
instance SEXPOf Float where
  sexp_of = SFloat
  
instance (Show v, Num v, Ord v, SEXPOf v) => SEXPOf (Complex v) where
  sexp_of = SComplex

instance (Show v, SEXPOf v) => SEXPOf (Ratio v) where
  sexp_of = SRatio

instance SEXPOf h => SEXPOf [h] where
  sexp_of = SList . map sexp_of

instance (SEXPOf a, SEXPOf b) => SEXPOf (a,b) where
  sexp_of (x,y) = SCons (sexp_of x) (sexp_of y)

instance (SEXPOf a, SEXPOf b, SEXPOf c) => SEXPOf (a,b,c) where
  sexp_of (x,y,z) = SList [sexp_of x, sexp_of y, sexp_of z]

instance (SEXPOf a, SEXPOf b, SEXPOf c, SEXPOf d)
      => SEXPOf (a,b,c,d) where
  sexp_of (x,y,z,w) = SList [sexp_of x, sexp_of y, sexp_of z, sexp_of w]

instance (SEXPOf a, SEXPOf b, SEXPOf c, SEXPOf d, SEXPOf e)
      => SEXPOf (a,b,c,d,e) where
  sexp_of (x,y,z,v,w) = SList [ sexp_of x
                              , sexp_of y
                              , sexp_of z
                              , sexp_of v
                              , sexp_of w ]

instance (SEXPOf k, SEXPOf v) => SEXPOf (Map k v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (IntMap v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (Set v) where
  sexp_of = sexp_of . toList

instance SEXPOf h => SEXPOf (Vector h) where
  sexp_of = SVector . fmap sexp_of

instance (SEXPOf i, SEXPOf h) => SEXPOf (Array i h) where
  -- not sure what to do here
  sexp_of xs = SList [SSymbol "array"
                     ,sexp_of (bounds xs)
                     ,sexp_of (fromList (elems xs))]

instance SEXPOf h => SEXPOf (Tree h) where
  sexp_of (Node x []) = sexp_of x
  sexp_of (Node x xs) = SCons (sexp_of x) (sexp_of xs)

instance Semigroup SEXP where
  SList xs <> SList ys = SList (xs <> ys)
  SList xs <> y = if null xs then y else SList (xs <> [y])
  x <> SList ys = if null ys then x else SList (x:ys)
  x <> y = SCons x y

instance Monoid SEXP where
  mempty = SList []

  
