import qualified Data.Random.Distribution.Uniform as UNI
import qualified Data.Packed.Vector as DPV
import qualified Numeric.Container as NC
import qualified Numeric.GSL.Minimization as GSL
import qualified Graphics.Rendering.Chart.Simple as CHART
import qualified Test.HUnit as HUNIT

-- Uniform CDF
uniformCDF a b x = UNI.realUniformCDF a b x
-- Uniform PDF
uniformPDF :: RealFrac a => a -> a -> a -> Double
uniformPDF a b x
  | b < a = uniformPDF b a x
  | x <= a || x >= b = 0
  | otherwise = realToFrac (1 / (b-a))

-- (Scalar) cost function
costFunc :: Double     -- lower bound on bids
  -> DPV.Vector Double -- vector of coefficients
  -> Double            -- bid value
  -> Double            -- corresponding cost value
costFunc bLow cs b =
  let k = DPV.dim cs
      bs = DPV.fromList $ zipWith (^) (take k [(b-bLow),(b-bLow)..]) [0..(k-1)]
  in bLow + NC.dot cs bs

-- Derivative of cost function
derivCostFunc :: Double -- lower bound on bids
  -> DPV.Vector Double  -- vector of coefficients
  -> Double             -- bids value
  -> Double             -- corresponding cost value
derivCostFunc bLow cs b =
  let k = DPV.dim cs
      ps = zipWith (^) (take k [(b-bLow),(b-bLow)..]) ([0] ++ [0..(k-2)])
      bs = DPV.fromList $ zipWith (\x y -> x * (fromIntegral y)) ps [0..(k-1)]
  in NC.dot cs bs

-- FoC vector function
focFunc :: Double       -- lower bound on bids
  -> DPV.Vector Double  -- vector of coefficients
  -> [Double -> Double] -- list of CDFs
  -> [Double -> Double] -- list of PDFs
  -> Double             -- bid value
  -> DPV.Vector Double  -- output FoC vector (to be minimized)
focFunc bLow cs cdfs pdfs b =
  let n = length cdfs
      m = (DPV.dim cs) `div` (fromIntegral n)
      indexes = [0,m..(m*(n-1))]
      vCs = map (\i -> DPV.subVector i m cs) indexes
      costs = map (\x -> costFunc bLow x b) vCs
      derivCosts = DPV.fromList $ map (\x -> derivCostFunc bLow x b) vCs
      zips = zip cdfs pdfs
      probs = DPV.fromList $ zipWith (\x (c,p) -> (1 - c x) / (p x)) costs zips
      rs = DPV.fromList $ map (\x -> 1 / (b - x)) costs
      consts = NC.constant ((sum $ DPV.toList rs) / ((fromIntegral n) - 1)) n
  in NC.sub derivCosts $ NC.mul probs $ NC.sub consts rs

-- Tests
-- Test costFunc
testCostFunc = HUNIT.TestCase (do
  let err = 1E-8
  let xs = [0.0,0.1..1.0]
  let expYs = map (\x -> x^2 - 0.5*x + 0.75) xs
  let ys = map (costFunc 0.5 (DPV.fromList [0.25,0.5,1.0])) xs
  let cmp = all (\x -> x == True) $ zipWith (\x y -> (abs (x-y)) < err) expYs ys
  HUNIT.assertBool "Testing costFunc: " cmp)
testDerivCostFunc = HUNIT.TestCase (do
  let err = 1E-8
  let xs = [0.0,0.1..1.0]
  let expYs = map (\x -> 2*x - 0.5) xs
  let ys = map (derivCostFunc 0.5 (DPV.fromList [0.25,0.5,1.0])) xs
  let cmp = all (\x -> x == True) $ zipWith (\x y -> (abs (x-y)) < err) expYs ys
  HUNIT.assertBool "Testing derivCostFunc: " cmp)

tests = HUNIT.TestList [HUNIT.TestLabel "testCostFunc" testCostFunc,
                        HUNIT.TestLabel "testDerivCostFunc" testDerivCostFunc]
