{-# LANGUAGE BangPatterns #-}
module Main where

-- friends
import Random.Array

import qualified Solver.Repa                    as R
import qualified Solver.Vector                  as V
import qualified Solver.Accelerate              as A

import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Execute
import Data.Array.Accelerate.LLVM.Native.State
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.IO                 as A
import Data.Array.Accelerate.LLVM.Native        as L
import qualified Data.Array.Accelerate.LLVM.Debug        as L


-- standard library
import Prelude                                  as P
import Data.List
import Control.Monad
import System.Environment
import System.Random.MWC
import qualified Data.Vector.Storable           as V
import qualified Data.Array.Repa                as R

import Criterion
import Criterion.Main
import Criterion.Monad
import Criterion.Config
import Criterion.Environment


doTest :: Config -> (String -> Bool) -> Environment -> Int -> IO ()
doTest cfg shouldRun env x =
  let n         = x * 1000000   -- x million
      name grp  = grp P.++ "/" P.++ shows x "M"
  in when (P.any shouldRun (P.map name ["vector", "repa", "cublas", "accelerate"])) $ do

    -- Generate random data
    xs_arr      <- randomArrayIO (const uniform) (Z :. n)
    ys_arr      <- randomArrayIO (const uniform) (Z :. n)

    let ((),xs_vec) = toVectors xs_arr
        ((),ys_vec) = toVectors ys_arr

    xs_repa     <- R.computeUnboxedP (R.delay $ toRepa xs_arr)
    ys_repa     <- R.computeUnboxedP (R.delay $ toRepa ys_arr)

    let !acc = convertAccWith config (A.dotp (A.use xs_arr) (A.use ys_arr))
    comp <- evalNative defaultTarget $ compileAcc acc

    -- Run the benchmark. A hacked up version of criterion's 'defaultMain' so
    -- that we don't continually call 'measureEnvironment'. All non-benchmarking
    -- paths are elided.
    --
    withConfig cfg $
      runAndAnalyse shouldRun env
      $ bgroup ""
        [ bench (name "vector")     $ nf (V.dotp xs_vec) ys_vec
        , bench (name "repa")       $ nfIO (R.dotp xs_repa ys_repa)
        , bench (name "accelerate-llvm (fusion)") $ whnf (\ys -> convertAccWith config (A.dotp (A.use xs_arr) (A.use ys))) ys_arr
        , bench (name "accelerate-llvm (compile)") $ whnfIO $ evalNative defaultTarget $ compileAcc acc
        , bench (name "accelerate-llvm (execute)") $ whnfIO $ evalNative defaultTarget $ executeAcc comp
        ]

config :: Phase
config =  Phase
  { recoverAccSharing      = True
  , recoverExpSharing      = True
  , floatOutAccFromExp     = True
  , enableAccFusion        = True
  , convertOffsetOfSegment = True
  }

main :: IO ()
main = do
--  L.setFlags [L.verbose, L.dump_llvm]
  -- Initialise the criterion environment
  (cfg, args)   <- parseArgs defaultConfig defaultOptions =<< getArgs
  env           <- withConfig cfg measureEnvironment

  let shouldRun b = P.null args || P.any (`isPrefixOf` b) args

  -- Setup and run the benchmark suite. This reinitialises the environment every
  -- time, but means we don't keep so much live memory around.
  --
  mapM_ (doTest cfg shouldRun env) [2]

