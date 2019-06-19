{-# language GADTs, TypeSynonymInstances, FlexibleInstances, LambdaCase, DeriveTraversable #-}

module Language.HExp where

import Data.String
import Data.Word
import Data.List
import Data.Foldable
import Data.Vector (Vector)

put_hexp :: SEXP -> IO ()
put_hexp = putStrLn . show

he1 :: SEXP
he1 = HExpL [HExpI 1, HExpI 2]
he2 :: SEXP
he2 = HExpC 'x'
he3 :: SEXP
he3 = HExpL [HExpC 'x', HExpC 'y']
he4 :: SEXP
he4 = HExpL [ HExpS "cat"
            , HExpL [HExpS "bird", HExpS "wolf"]]
he5 = HExpP he2 he3
he6 = HExpP (HExpS "cat") (HExpI 12)

ghcid = do traverse_ put_hexp [he1,he2,he3,he4,he5,he6]

data SEXP where
  HExpB :: Bool -> SEXP
  HExpI :: Integer -> SEXP
  HExpC :: Char -> SEXP
  HExpS :: String -> SEXP
  HExpL :: [SEXP] -> SEXP
  HExpP :: SEXP -> SEXP -> SEXP
  HExpV :: Vector SEXP -> SEXP

instance Show SEXP where
  show = \case
    HExpL xs -> '(' : (unwords $ show <$> xs) <> ")"
    HExpB b -> if b then "#t" else "#f"
    HExpI x -> show x
    HExpC x -> "#\\" <> [x]
    HExpS x -> x
    HExpP x y -> case y of
      HExpL ys -> show (HExpL (x:ys))
      _ -> '(' : unwords [show x, ".", show y]  <> ")"
    HExpV v -> '#' : show (HExpL (toList v)) 

class AsSEXP h where
  sexp_of :: h -> SEXP

instance AsSEXP Bool where
  sexp_of = HExpB

instance AsSEXP Char where
  sexp_of = HExpC

instance AsSEXP Int where
  sexp_of = HExpI . fromIntegral

instance AsSEXP Word where
  sexp_of = HExpI . fromIntegral

instance AsSEXP Integer where
  sexp_of = HExpI

instance AsSEXP h => AsSEXP [h] where
  sexp_of = HExpL . map sexp_of

instance (AsSEXP a, AsSEXP b) => AsSEXP (a,b) where
  sexp_of (x,y) = HExpP (sexp_of x) (sexp_of y)


    --instance Semigroup (SEXP a) where
--  a@Atom{} <> b@Atom{} = List [a,b]
--  a@Atom{} <> List xs = List (a:xs)
--  List xs <> b@Atom{} = List (xs <> [b])
--  List xs <> List ys = List (xs <> ys)
--
--instance Monoid (SEXP a) where
--  mempty = List []
    
