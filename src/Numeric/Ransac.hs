{-# LANGUAGE BangPatterns #-}
-- | The RANdom SAmple Consensus (RANSAC) algorithm for estimating the
-- parameters of a mathematical model from a data set. See
-- <http://en.wikipedia.org/wiki/RANSAC> for more information.
module Numeric.Ransac (ransac) where
import Control.Applicative
import Control.Monad (replicateM)
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as V
import System.Random

--randDistinct :: (Eq a, Random a, Monad m) => Int -> m a -> m [a]
randDistinct :: Int -> IO Int -> IO [Int]
randDistinct n gen = go 0 [] []
  where go !i acc _ | i == n = return acc
        go !i acc [] = replicateM (n-i) gen >>= go i acc
        go !i acc (r:rs) = if r `elem` acc 
                           then go i acc rs 
                           else go (i+1) (r:acc) rs
{- SPECIALIZE randDistinct :: Int -> IO Int -> IO [Int] #-}

untilJust :: Monad m => m (Maybe b) -> m b
untilJust x = go 
  where go = x >>= maybe go return
{-# INLINE untilJust #-}

-- | @ransac iter sampleSize agreePct fit residual goodFit pts@ draws
-- @iter@ samples of size @sampleSize@ from @pts@. The @fit@ function
-- is used to produce a model from each of these samples. The elements
-- of @pts@ whose residuals pass the @goodFit@ predicate with respect
-- to this model are identified as /inliers/, and used to update the
-- model. The model for which the size of the inliers set is at least
-- @agreePct@ percent of the entire data set and whose error over all
-- points is minimal among all sampled models is returned. If no
-- acceptable model is found (i.e. no model whose inliers were at
-- least @agreePct@ percent of the entire data set), 'Nothing' is
-- returned.
ransac :: (V.Vector v a, V.Vector v d, Num d, Ord d) => 
          Int -> Int -> Float -> 
          (v a -> Maybe c) -> (c -> a -> d) -> (d -> Bool) -> 
          v a -> IO (Maybe (c, v a))
ransac maxIter sampleSize agree fit residual goodFit pts = genModel >>= go 0
  where go i r@(model, bestError, inliers)
          | i == maxIter = if ratioInliers (V.length inliers) < agree
                           then return Nothing
                           else return (Just (model, inliers))
          | otherwise = do r'@(_, err, inliers') <- genModel
                           if ratioInliers (V.length inliers') >= agree
                              && err < bestError
                           then go (i+1) r'
                           else go (i+1) r
        sample = V.fromList . map (pts !) <$> 
                 randDistinct sampleSize ((`rem` n) . abs <$> randomIO)
        genModel = do model <- untilJust (fit <$> sample)
                      let !errors = V.map (residual model) pts
                          !inliers = V.ifilter (const . goodFit . (errors !)) pts
                      case fit inliers of
                        Nothing -> genModel
                        Just model' -> let err = V.sum $ V.map (residual model') pts
                                       in return (model', err, inliers)
        n = V.length pts
        ratioInliers n' = fromIntegral n' / fromIntegral n
{-# INLINE ransac #-}
