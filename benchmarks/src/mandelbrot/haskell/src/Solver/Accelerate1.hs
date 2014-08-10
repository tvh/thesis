{-# LANGUAGE ScopedTypeVariables #-}

module Solver.Accelerate1 (

  mandelbrot

) where

import Prelude                                  as P
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.Math.Complex

import Solver.Accelerate.Common


-- Mandelbrot Set --------------------------------------------------------------

-- Compute the mandelbrot as repeated application of the recurrence relation:
--
--   Z_{n+1} = c + Z_n^2
--
mandelbrot
    :: forall a. (Elt a, IsFloating a)
    => Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> Acc Bitmap
mandelbrot screenX screenY depth view
  = A.map (prettyRGBA (P.fromIntegral depth))
  $ P.snd . A.unzip
  $ P.foldr ($) zs0
  $ P.take depth (repeat step)
  where
    -- The view plane
    (xmin,ymin,xmax,ymax)     = unlift (the view)
    sizex                     = xmax - xmin
    sizey                     = ymax - ymin

    viewx                     = constant (P.fromIntegral screenX)
    viewy                     = constant (P.fromIntegral screenY)

    -- take a single step of the iteration
    step :: Acc (Array DIM2 (Complex a, Int32))
         -> Acc (Array DIM2 (Complex a, Int32))
    step = A.zipWith iter cs                    -- compute one iteration to progress to Z_{n+1}

    -- initial conditions for a given pixel in the window, translated to the
    -- corresponding point in the complex plane
    cs  = A.generate (constant $ Z :. screenY :. screenX) initial       -- the static complex plane $c$
    zs0 = A.map (\c -> lift (c, constant 0)) cs                         -- initial array of $(c, 0)$

    initial :: Exp DIM2 -> Exp (Complex a)
    initial ix = lift ( (xmin + (x * sizex) / viewx) :+ (ymin + (y * sizey) / viewy) )
      where
        pr = unindex2 ix
        x  = A.fromIntegral (A.snd pr :: Exp Int)
        y  = A.fromIntegral (A.fst pr :: Exp Int)

    -- take a single step of the iteration
    iter :: Exp (Complex a) -> Exp (Complex a, Int32) -> Exp (Complex a, Int32)
    iter c zi = next (A.fst zi) (A.snd zi)
     where
      next :: Exp (Complex a) -> Exp Int32 -> Exp (Complex a, Int32)
      next z i =
        let z' = c + z*z
        in (dot z' >* 4) ? ( zi , lift (z', i+1) )

    dot :: Exp (Complex a) -> Exp a
    dot c = let r :+ i = unlift c
            in  r*r + i*i

