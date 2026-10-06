{-# LANGUAGE PackageImports #-}
module ImportWithPackageImport where

import "base" Data.List (intersperse)
import qualified Data.Map as Map
import ExplicitA ( a1, a2 )

ordinaryMap :: Map.Map String String
ordinaryMap = Map.fromList [(a1, a2)]

main :: IO ()
main =
    putStrLn (concat (intersperse " " ["hello", "world", name, "!"]))
  where
    name =
      Map.findWithDefault "default" a1 ordinaryMap
