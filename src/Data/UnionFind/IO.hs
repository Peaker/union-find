module Data.UnionFind.IO
  ( Point, fresh, repr, union, union', equivalent, redundant,
    descriptor, setDescriptor, modifyDescriptor )
where

import Data.IORef
import Data.UnionFind.Monadic (fresh, repr, union, union', equivalent, redundant, descriptor, setDescriptor, modifyDescriptor)
import qualified Data.UnionFind.Monadic as UF

type Point = UF.Point IORef
