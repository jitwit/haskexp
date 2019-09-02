

module Main where

import Language.SEXP
import qualified Data.ByteString.Lazy as S
import Data.Aeson

main :: IO ()
main = print . sexp_of . (decode :: S.ByteString -> Maybe Value) =<< S.getContents
  
  
