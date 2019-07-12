{-# language TypeApplications #-}

module Test where

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
import Language.SEXP

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

ghcid = traverse_ put_sexp hexps1

