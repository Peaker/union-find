module Data.UnionFind.ST
  ( Point, fresh, repr, union, union', equivalent, redundant,
    descriptor, setDescriptor, modifyDescriptor )
where

import Data.STRef
import Data.UnionFind.Monadic (fresh, repr, union, union', equivalent, redundant, descriptor, setDescriptor, modifyDescriptor)
import qualified Data.UnionFind.Monadic as UF

type Point s = UF.Point (STRef s)

