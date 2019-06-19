{-# language GADTs, LambdaCase, MultiWayIf #-}

module Language.SEXP where

import Data.String
import Data.Word
import Data.List
import Data.Foldable
import Data.Vector (Vector,fromList)
import Data.Array (Array,listArray,bounds,elems)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import Data.IntSet (IntSet)
import qualified Data.IntSet (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.Complex
import Data.Tree
import GHC.Real

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
  SByteString :: ByteString -> SEXP
  SText :: Text -> SEXP
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
    SByteString x -> show x
    SText x -> show x
    SDouble x -> show x
    SFloat x -> show x
    SRatio (x :% y) -> concat [show x,"/",show y]
    SComplex (x :+ y) ->
      if | y > 0 -> show x <> "+" <> show y <> "i"
         | y < 0 -> show x <> "-" <> show (-y) <> "i"
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

instance SEXPOf ByteString where
  sexp_of = SByteString

instance SEXPOf Text where
  sexp_of = SText

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
  sexp_of (x,y,z,v,w) = SList
    [ sexp_of x, sexp_of y, sexp_of z, sexp_of v, sexp_of w ]

instance (SEXPOf k, SEXPOf v) => SEXPOf (Map k v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (IntMap v) where
  sexp_of = sexp_of . toList

instance SEXPOf v => SEXPOf (Set v) where
  sexp_of = sexp_of . toList

instance SEXPOf IntSet where
  sexp_of = sexp_of . Data.IntSet.toList

instance SEXPOf h => SEXPOf (Vector h) where
  sexp_of = SVector . fmap sexp_of

instance SEXPOf h => SEXPOf (Seq h) where
  sexp_of = sexp_of . toList

instance (SEXPOf i, SEXPOf h) => SEXPOf (Array i h) where
  -- not sure what to do here
  sexp_of xs = SList [SSymbol "array"
                     ,sexp_of (bounds xs)
                     ,sexp_of (fromList (elems xs))]

instance (SEXPOf k, SEXPOf v) => SEXPOf (HashMap k v) where
  sexp_of = sexp_of . toList

instance SEXPOf k => SEXPOf (HashSet k) where
  sexp_of = sexp_of . toList

instance SEXPOf h => SEXPOf (Tree h) where
  sexp_of (Node x []) = sexp_of x
  sexp_of (Node x xs) = SCons (sexp_of x) (sexp_of xs)

instance SEXPOf h => SEXPOf (Maybe h) where
  -- truthy
  sexp_of = maybe (SBool False) sexp_of

instance Semigroup SEXP where
  SList xs <> SList ys = SList (xs <> ys)
  SList xs <> y = if null xs then y else SList (xs <> [y])
  x <> SList ys = if null ys then x else SList (x:ys)
  x <> y = SCons x y

instance Monoid SEXP where
  mempty = sexp_of ()

  
