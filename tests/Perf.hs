{-# LANGUAGE BangPatterns #-}
module Main (main) where
import Control.Applicative
import Control.Lens (view)
import Criterion.Main
import Data.Accessor ((^=))
import Data.Colour (opaque)
import Data.Colour.Names
import qualified Data.Foldable as F
import Data.Random.Normal (normalsIO')
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Linear
import Numeric.Ransac

type Point = V2 Float

fitLine :: Vector Point -> Maybe (V2 Float)
fitLine pts = (!* b) <$> inv22 a
  where sx = V.sum $ V.map (view _x) pts
        a = V2 (V2 (V.sum (V.map ((^2).view _x) pts)) sx)
               (V2 sx (fromIntegral $ V.length pts))
        b = V2 (V.sum (V.map F.product pts))
               (V.sum (V.map (view _y) pts))

ptError :: V2 Float -> Point -> Float
ptError (V2 m b) (V2 x y) = sq $ y - (m*x+b)
  where sq x = x * x

main = do noise <- v2Cast . V.fromList . take (n*2) <$> normalsIO' (0,0.3)
          let !pts' = V.zipWith (+) noise pts
              ran = ransac 100 2 0.6 fitLine ptError (< 1) pts'
          putStr $ "Sanity "
          ran >>= putStrLn . show . fmap fst
          defaultMain [ bench "linear fit" $ fmap (quadrance . fst) <$> ran ]
  where n = 10000
        !pts = V.generate n mkPt
        mkPt :: Int -> V2 Float
        mkPt i = let x = fromIntegral i / 500
                 in V2 x (3*x + 2)
        v2Cast :: Vector Float -> Vector Point
        v2Cast = V.unsafeCast

        