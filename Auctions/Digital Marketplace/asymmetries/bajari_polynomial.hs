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

-- Cost function
costFunc :: Double -> DPV.Vector Double -> Double -> Double
costFunc bLow cs b =
  let k = DPV.dim cs
      bs = DPV.fromList $ zipWith (^) (take k [(b-bLow),(b-bLow)..]) [0..(k-1)]
  in bLow + NC.dot cs bs

-- Derivative of cost function
derivCostFunc :: Double -> DPV.Vector Double -> Double -> Double
derivCostFunc bLow cs b =
  let k = DPV.dim cs
      ps = zipWith (^) (take k [(b-bLow),(b-bLow)..]) ([0] ++ [0..(k-2)])
      bs = DPV.fromList $ zipWith (\x y -> x * (fromIntegral y)) ps [0..(k-1)]
  in NC.dot cs bs

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
