import qualified Numeric.GSL.ODE as ODE
import qualified Data.Packed.Vector as DPV
import qualified Numeric.Container as NC
import qualified Data.Random.Distribution.Uniform as UNI
import qualified Graphics.Plot as GP

-- Uniform CDF
uniformCDF :: RealFrac a => a -> a -> a -> Double
uniformCDF = UNI.realUniformCDF

-- Lower extremities
lowerExt :: Double -- price weight (w)
  -> [Double]      -- list of reputations
  -> [Double]      -- corresponding list of lower extremities
lowerExt w = map (\r -> (1-w)*r)

-- Upper extremities
upperExt :: Double -- price weight (w)
  -> [Double]      -- list of reputations
  -> [Double]      -- corresponding list of upper extremities
upperExt w reps = map (+w) $ lowerExt w reps

-- Upper bound on bids
upperBoundBidsFunc :: [Double] -- list of lower extremities
  -> [Double]                  -- list of upper extremities
  -> Double                    -- output estimate on upper bound on bids
upperBoundBidsFunc lowers uppers =
  let bs = NC.linspace 10000 (head uppers, uppers !! 1)
      cdfs = zipWith uniformCDF (drop 1 lowers) (drop 1 uppers)
      negCdfs x = map (\cdf -> 1 - cdf x) cdfs
      objective x = (x - head uppers) * product (negCdfs x)
  in NC.atIndex bs $ NC.maxIndex $ DPV.mapVector objective bs

-- FoC vector function
focFunc :: 
  DPV.Vector Double     -- vector of upper extremities
  -> Double             -- independent variable
  -> DPV.Vector Double  -- vector of inputs
  -> DPV.Vector Double  -- vector of derivatives
focFunc uppers t ys =
  let n = DPV.dim ys
      probV = DPV.zipVectorWith (-) uppers ys
      rsV = DPV.mapVector (\x -> 1 / (t - x)) ys
      constV = NC.constant (sum (DPV.toList rsV) / (fromIntegral n - 1)) n
  in NC.mul probV $ NC.sub constV rsV

-- Main
main :: IO ()
main = do
  let w = 0.5
  let reps = [0.25, 0.75]
  let lowers = lowerExt w reps
  let uppers = upperExt w reps
  let bLow = 0.5178175311899669
  let bUpper = upperBoundBidsFunc lowers uppers
  let xdot = focFunc (DPV.fromList uppers)
  let ts = NC.linspace 100 (bLow, bUpper)
  let s = ODE.odeSolveV ODE.RKf45 0.01 1E-8 1E-8 xdot (DPV.fromList lowers) ts
  GP.mplot (ts : NC.toColumns s)
