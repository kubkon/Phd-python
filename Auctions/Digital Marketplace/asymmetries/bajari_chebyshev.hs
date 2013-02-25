import qualified Data.Random.Distribution.Uniform as UNI
import qualified Data.Packed.Vector as DPV
import qualified Numeric.Container as NC
import qualified Numeric.GSL.Minimization as GSL
import qualified Math.Polynomial.Chebyshev as MPC
import qualified Test.HUnit as HUNIT
import qualified Data.String.Utils as UTILS

-- Uniform CDF
uniformCDF :: RealFrac a => a -> a -> a -> Double
uniformCDF = UNI.realUniformCDF

-- Uniform PDF
uniformPDF :: RealFrac a => a -> a -> a -> Double
uniformPDF a b x
  | b < a = uniformPDF b a x
  | x <= a || x >= b = 0
  | otherwise = realToFrac (1 / (b-a))

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

-- Chebyshev series of the first kind at x
evalFirstKind :: DPV.Vector Double -- vector of coefficients
  -> Double                        -- x (lying within [-1,1])
  -> Double                        -- corresponding sum of the series
evalFirstKind cs x =
  let coeffs = DPV.toList cs
      series = MPC.evalTs x
  in sum $ zipWith (*) coeffs series

-- Chebyshev series of the second kind at x
evalSecondKind :: DPV.Vector Double -- vector of coefficients
  -> Double                         -- x (lying within [-1,1])
  -> Double                         -- corresponding sum of the series
evalSecondKind cs x =
  let coeffs = DPV.toList cs
      series = MPC.evalUs x
  in sum $ zipWith (*) coeffs series

-- Shift Chebyshev domain
shiftChebyshev :: (Double, Double) -- bounds on bids
  -> Double                        -- x in original domain
  -> Double                        -- x' in Chebyshev domain ([-1,1])
shiftChebyshev (bLow, bUpper) x = (2*x - bUpper - bLow) / (bUpper - bLow)

-- (Scalar) cost function
costFunc :: Double     -- lower extremity
  -> (Double, Double)  -- bounds on bids
  -> DPV.Vector Double -- vector of coefficients
  -> Double            -- bid value
  -> Double            -- corresponding cost value
costFunc l (bLow, bUpper) cs b =
  let series = evalFirstKind cs $ shiftChebyshev (bLow, bUpper) b
  in (bUpper - b) / (bUpper - bLow) * l + (b - bLow) * series

-- Derivative of cost function
derivCostFunc :: Double -- lower extremity
  -> (Double, Double)   -- bounds on bids
  -> DPV.Vector Double  -- vector of coefficients
  -> Double             -- bids value
  -> Double             -- corresponding cost value
derivCostFunc l bBounds@(bLow, bUpper) cs b =
  let firstSeries = evalFirstKind cs $ shiftChebyshev bBounds b
      cs1 = drop 1 $ DPV.toList cs
      coeffs = DPV.fromList $ zipWith (*) cs1 [1.0..fromIntegral $ length cs1]
      secondSeries = evalSecondKind coeffs $ shiftChebyshev bBounds b
  in firstSeries + 2*(b - bLow) / (bUpper - bLow) * secondSeries - l / (bUpper - bLow)

-- FoC vector function
focFunc :: [Double]      -- list of lower extremities
  -> (Double, Double)    -- bounds on bids
  -> [DPV.Vector Double] -- list of vector of coefficients
  -> [Double -> Double]  -- list of CDFs
  -> [Double -> Double]  -- list of PDFs
  -> Double              -- bid value
  -> DPV.Vector Double   -- output FoC vector (to be minimized)
focFunc lowers bBounds vCs cdfs pdfs b =
  let n = length cdfs
      costs = map (\(l,x) -> costFunc l bBounds x b) $ zip lowers vCs
      derivCosts = DPV.fromList $ map (\(l,x) -> derivCostFunc l bBounds x b) $ zip lowers vCs
      zips = zip cdfs pdfs
      probs = DPV.fromList $ zipWith (\x (c,p) -> (1 - c x) / p x) costs zips
      rs = DPV.fromList $ map (\x -> 1 / (b - x)) costs
      consts = NC.constant (sum (DPV.toList rs) / (fromIntegral n - 1)) n
  in NC.sub derivCosts $ NC.mul probs $ NC.sub consts rs

-- Lower boundary condition vector function
lowerBoundFunc :: [Double] -- list of lower extremities
  -> (Double, Double)      -- bounds on bids
  -> [DPV.Vector Double]   -- list of vector of coefficients
  -> DPV.Vector Double     -- output lower boundary vector (to be minimized)
lowerBoundFunc lowers (bLow, bUpper) vCs =
  let costs = DPV.fromList $ map (\(l,x) -> costFunc l (bLow, bUpper) x bLow) $ zip lowers vCs
      consts = DPV.fromList lowers
  in NC.sub costs consts

-- Upper boundary condition vector function
upperBoundFunc :: [Double] -- list of lower extremities
  -> (Double, Double)      -- bounds on bids
  -> [DPV.Vector Double]   -- list of vector of coefficients
  -> DPV.Vector Double     -- output upper boundary vector (to be minimized)
upperBoundFunc lowers (bLow, bUpper) vCs =
  let n = length vCs
      costs = DPV.fromList $ map (\(l,x) -> costFunc l (bLow, bUpper) x bUpper) $ zip lowers vCs
      consts = NC.constant bUpper n
  in NC.sub costs consts

-- Objective function
objFunc :: Int      -- grid granularity
  -> Double         -- upper bound on bids
  -> [Double]       -- list of lower extremities
  -> [Double]       -- list of upper extremities
  -> [Double]       -- parameters to estimate
  -> Double         -- value of the objective
objFunc granularity bUpper lowers uppers params =
  let bLow = head params
      cs = DPV.fromList $ drop 1 params
      n = length cdfs
      m = DPV.dim cs `div` fromIntegral n
      indexes = [0,m..(m*(n-1))]
      vCs = map (\i -> DPV.subVector i m cs) indexes
      cdfs = zipWith uniformCDF lowers uppers
      pdfs = zipWith uniformPDF lowers uppers
      bs = NC.linspace granularity (bLow, bUpper)
      focSq b = NC.sumElements $ DPV.mapVector (**2) $ focFunc lowers (bLow, bUpper) vCs cdfs pdfs b
      foc = NC.sumElements $ DPV.mapVector focSq bs
      lowerBound = NC.sumElements $ DPV.mapVector (**2) $ lowerBoundFunc lowers (bLow, bUpper) vCs
      upperBound = NC.sumElements $ DPV.mapVector (**2) $ upperBoundFunc lowers (bLow, bUpper) vCs
  in foc + fromIntegral granularity * lowerBound + fromIntegral granularity * upperBound

{-
  Impure (main) program goes here
-}
-- Minimization
main :: IO ()
main = do
  let w = 0.5
  let reps = [0.25, 0.75]
  let n = length reps
  let numCoeffs = 5
  let lowers = lowerExt w reps
  let uppers = upperExt w reps
  let bUpper = upperBoundBidsFunc lowers uppers
  let granularity = 40
  let objective = objFunc granularity bUpper lowers uppers
  let l1 = lowers !! 1
  let initSizeBox = take (n*numCoeffs + 1) [1E-2,1E-2..]
  let initConditions = take (n*numCoeffs + 1) (l1 : [1E-2,1E-2..])
  let (s,_) = GSL.minimize GSL.NMSimplex2 1E-8 100000 initSizeBox objective initConditions
  let bLow = head s
  let vCs = DPV.fromList $ drop 1 s
  let indexes = [0,numCoeffs..(numCoeffs*(n-1))]
  let cs = map (DPV.toList . (\i -> DPV.subVector i numCoeffs vCs)) indexes
  let filePath = "chebyshev.out"
  let fileContents = UTILS.join "\n" [
        UTILS.join " " (["w", "reps", "b_lower", "b_upper"] ++ [UTILS.join "_" ["cs", show i] | i <- [0..n]]),
        UTILS.join " " ([show w, show reps, show bLow, show bUpper] ++ [show c | c <- cs])]
  writeFile filePath fileContents

{-
  Specification of tests goes here
-}
-- Test evalFirstKind
testEvalFirstKind :: HUNIT.Test
testEvalFirstKind = HUNIT.TestCase (do
  let xs = [-1.0,-0.95..1.0]
  let cs = DPV.fromList [0.1..0.9]
  let expYs = map (sum . zipWith (*) (DPV.toList cs) . MPC.evalTs) xs
  let ys = map (evalFirstKind cs) xs
  HUNIT.assertEqual "Testing evalFirstKind: " expYs ys)
-- Test evalSecondKind
testEvalSecondKind :: HUNIT.Test
testEvalSecondKind = HUNIT.TestCase (do
  let xs = [-1.0,-0.95..1.0]
  let cs = DPV.fromList [0.1..0.9]
  let expYs = map (sum . zipWith (*) (DPV.toList cs) . MPC.evalUs) xs
  let ys = map (evalSecondKind cs) xs
  HUNIT.assertEqual "Testing evalSecondKind: " expYs ys)

tests :: HUNIT.Test
tests = HUNIT.TestList [HUNIT.TestLabel "testEvalFirstKind" testEvalFirstKind,
                        HUNIT.TestLabel "testEvalSecondKind" testEvalSecondKind]
