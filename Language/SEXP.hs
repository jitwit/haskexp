{-# language GADTs, LambdaCase, MultiWayIf #-}

module Language.SEXP
  ( SEXP (..)
  , Sexpressive (..)
  , put_sexp
  , put_sexp'
  ) where

import Control.Monad
import Data.Word
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

class Sexpressive h where
  sexp_of :: h -> SEXP

instance Sexpressive () where
  sexp_of () = SList []

instance Sexpressive Bool where
  sexp_of = SBool

instance Sexpressive Char where
  sexp_of = SChar

instance Sexpressive Int where
  sexp_of = SInteger . fromIntegral

instance Sexpressive Word where
  sexp_of = SInteger . fromIntegral

instance Sexpressive Integer where
  sexp_of = SInteger

instance Sexpressive Double where
  sexp_of = SDouble

instance Sexpressive Float where
  sexp_of = SFloat

instance Sexpressive ByteString where
  sexp_of = SByteString

instance Sexpressive Text where
  sexp_of = SText

instance (Show v, Num v, Ord v, Sexpressive v) => Sexpressive (Complex v) where
  sexp_of = SComplex

instance (Show v, Sexpressive v) => Sexpressive (Ratio v) where
  sexp_of = SRatio

instance Sexpressive h => Sexpressive [h] where
  sexp_of = SList . map sexp_of

instance (Sexpressive a, Sexpressive b) => Sexpressive (a,b) where
  sexp_of (x,y) = SCons (sexp_of x) (sexp_of y)

instance (Sexpressive a, Sexpressive b, Sexpressive c) => Sexpressive (a,b,c) where
  sexp_of (x,y,z) = SList [sexp_of x, sexp_of y, sexp_of z]

instance (Sexpressive a, Sexpressive b, Sexpressive c, Sexpressive d)
      => Sexpressive (a,b,c,d) where
  sexp_of (x,y,z,w) = SList [sexp_of x, sexp_of y, sexp_of z, sexp_of w]

instance (Sexpressive a, Sexpressive b, Sexpressive c, Sexpressive d, Sexpressive e)
      => Sexpressive (a,b,c,d,e) where
  sexp_of (x,y,z,v,w) = SList
    [ sexp_of x, sexp_of y, sexp_of z, sexp_of v, sexp_of w ]

instance ( Sexpressive a
         , Sexpressive b
         , Sexpressive c
         , Sexpressive d
         , Sexpressive e
         , Sexpressive f)
      => Sexpressive (a,b,c,d,e,f) where
  sexp_of (x,y,z,u,v,w) = SList
    [ sexp_of x, sexp_of y, sexp_of z, sexp_of u, sexp_of v, sexp_of w ]

instance (Sexpressive k, Sexpressive v) => Sexpressive (Map k v) where
  sexp_of = sexp_of . toList

instance Sexpressive v => Sexpressive (IntMap v) where
  sexp_of = sexp_of . toList

instance Sexpressive v => Sexpressive (Set v) where
  sexp_of = sexp_of . toList

instance Sexpressive IntSet where
  sexp_of = sexp_of . Data.IntSet.toList

instance Sexpressive h => Sexpressive (Vector h) where
  sexp_of = SVector . fmap sexp_of

instance Sexpressive h => Sexpressive (Seq h) where
  sexp_of = sexp_of . toList

instance (Sexpressive i, Sexpressive h) => Sexpressive (Array i h) where
  -- not sure what to do here
  sexp_of xs = SList [SSymbol "array"
                     ,sexp_of (bounds xs)
                     ,sexp_of (fromList (elems xs))]

instance (Sexpressive k, Sexpressive v) => Sexpressive (HashMap k v) where
  sexp_of = sexp_of . toList

instance Sexpressive k => Sexpressive (HashSet k) where
  sexp_of = sexp_of . toList

instance Sexpressive h => Sexpressive (Tree h) where
  sexp_of (Node x []) = sexp_of x
  sexp_of (Node x xs) = SCons (sexp_of x) (sexp_of xs)

instance Sexpressive h => Sexpressive (Maybe h) where
  -- truthy
  sexp_of = maybe (SBool False) sexp_of

scons :: SEXP -> SEXP -> SEXP
scons (SList []) y = y
scons x (SList []) = x
scons (SList xs) y = SList (xs <> [y])
scons x (SList ys) = SList (x:ys)
scons x y = SCons x y

instance Semigroup SEXP where
  (<>) = scons

instance Monoid SEXP where
  mempty = sexp_of ()

put_sexp :: SEXP -> IO ()
put_sexp = putStrLn . show

put_sexp' :: Sexpressive h => h -> IO ()
put_sexp' = put_sexp . sexp_of


