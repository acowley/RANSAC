-- |Basic linear fit unit test. There is no noise in the data set, so
-- it is very unlikely that the correct model will not be found.
module Main where
import Control.Applicative
import Control.Lens (view)
import qualified Data.Foldable as F
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Linear
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test, (~?))
import Numeric.Ransac

type Point = V2 Float

sq :: Float -> Float
sq x = x * x

-- | Fit a 2D line to a collection of 'Point's.
fitLine :: Vector Point -> Maybe (V2 Float)
fitLine pts = (!* b) <$> inv22 a
  where sx = V.sum $ V.map (view _x) pts
        a = V2 (V2 (V.sum (V.map (sq . view _x) pts)) sx)
               (V2 sx (fromIntegral (V.length pts)))
        b = V2 (V.sum (V.map F.product pts))
               (V.sum (V.map (view _y) pts))

-- | Compute the error of a 'Point' with respect to a hypothesized
-- linear model.
ptError :: V2 Float -> Point -> Float
ptError (V2 m b) (V2 x y) = sq $ y - (m*x+b)

test1 :: Test
test1 = tst ~? "noise-free linear fit"
  where model = ransac 20 2 0.8 fitLine ptError (< 0.1) pts
        pts :: Vector Point
        pts = V.generate 100 mkPoint
        mkPoint i = let x = fromIntegral i * 0.1
                    in V2 x (3 * x + 1)
        tst = do mm <- model
                 return $ case mm of
                   Nothing -> False
                   Just (m,_) -> qd (V2 3 1) m < 0.1

main :: IO ()
main = defaultMain $ hUnitTestToTests test1