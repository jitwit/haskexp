{-# language GADTs, TypeSynonymInstances, FlexibleInstances, LambdaCase, DeriveTraversable #-}

module Language.HExp where

import Data.String
import Data.Word
import Data.List
import Data.Foldable

put_hexp :: Show a => SEXP a -> IO ()
put_hexp = putStrLn . show

he1 :: SEXP Int
he1 = List $ Single <$> [AtomI 1, AtomI 2]
he2 :: SEXP Char
he2 = Single $ AtomC 'x'
he3 :: SEXP Char
he3 = List $ Single <$> [AtomC 'x', AtomC 'y']
he4 :: SEXP String
he4 = List [ Single $ AtomS "cat"
           , List (Single <$> [AtomS "bird", AtomS "wolf"])]

ghcid = do traverse_ put_hexp [he1]
           traverse_ put_hexp [he2,he3]
           traverse_ put_hexp [he4]

data Atom a where
  AtomB :: Bool -> Atom Bool
  AtomI :: Int -> Atom Int
  AtomC :: Char -> Atom Char
  AtomS :: String -> Atom String
--  AtomL :: [SEXP a]

data SEXP a = Single (Atom a) | List [SEXP a]

instance Show a => Show (SEXP a) where
  show = \case
    List xs -> '(' : (unwords $ show <$> xs) <> ")"
    Single a -> case a of
      AtomB b -> if b then "#t" else "#f"
      AtomI x -> show x
      AtomC x -> "#\\" <> [x]
      AtomS x -> x


--instance Semigroup (SEXP a) where
--  a@Atom{} <> b@Atom{} = List [a,b]
--  a@Atom{} <> List xs = List (a:xs)
--  List xs <> b@Atom{} = List (xs <> [b])
--  List xs <> List ys = List (xs <> ys)
--
--instance Monoid (SEXP a) where
--  mempty = List []
    
