module Main where

import Language.SEXP
import Data.ByteString.Lazy as S
import Data.Aeson

main :: IO ()
main = put_sexp' . (decode :: S.ByteString -> Maybe Value) =<< S.getContents
  
  
