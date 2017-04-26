module Accelerate where

import Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native  as CPU
--import Data.Array.Accelerate.LLVM.PTX     as GPU


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

xs = fromList (Z:.50000000) [0..]   :: Vector Float
ys = fromList (Z:.50000000) [1,3..] :: Vector Float

cpuresult = CPU.run $ dotp (use xs) (use ys)
--gpuresult = GPU.run $ dotp (use xs) (use ys)
