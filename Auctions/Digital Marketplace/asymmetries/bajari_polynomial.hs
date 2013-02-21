import qualified Data.Random.Distribution.Uniform as UNI
import qualified Data.Packed.Vector as DPV
import qualified Numeric.Container as NC
import qualified Numeric.GSL.Minimization as GSL
import qualified Graphics.Rendering.Chart.Simple as CHART
import qualified Test.HUnit as HUNIT

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
      ps = zipWith (^) (take k [(b-bLow),(b-bLow)..]) (0 : [0..(k-2)])
      bs = DPV.fromList $ zipWith (\x y -> x * fromIntegral y) ps [0..(k-1)]
  in NC.dot cs bs

-- FoC vector function
focFunc :: Double        -- lower bound on bids
  -> [DPV.Vector Double] -- list of vector of coefficients
  -> [Double -> Double]  -- list of CDFs
  -> [Double -> Double]  -- list of PDFs
  -> Double              -- bid value
  -> DPV.Vector Double   -- output FoC vector (to be minimized)
focFunc bLow vCs cdfs pdfs b =
  let n = length cdfs
      costs = map (\x -> costFunc bLow x b) vCs
      derivCosts = DPV.fromList $ map (\x -> derivCostFunc bLow x b) vCs
      zips = zip cdfs pdfs
      probs = DPV.fromList $ zipWith (\x (c,p) -> (1 - c x) / p x) costs zips
      rs = DPV.fromList $ map (\x -> 1 / (b - x)) costs
      consts = NC.constant (sum (DPV.toList rs) / (fromIntegral n - 1)) n
  in NC.sub derivCosts $ NC.mul probs $ NC.sub consts rs

-- Lower boundary condition vector function
lowerBoundFunc :: Double -- lower bound on bids
  -> [DPV.Vector Double] -- list of vector of coefficients
  -> [Double]            -- list of lower extremities
  -> DPV.Vector Double   -- output lower boundary vector (to be minimized)
lowerBoundFunc bLow vCs lowers =
  let costs = DPV.fromList $ map (\x -> costFunc bLow x bLow) vCs
      consts = DPV.fromList lowers
  in NC.sub costs consts

-- Upper boundary condition vector function
upperBoundFunc :: Double -- lower bound on bids
  -> Double              -- upper bound on bids
  -> [DPV.Vector Double] -- list of vector of coefficients
  -> DPV.Vector Double   -- output upper boundary vector (to be minimized)
upperBoundFunc bLow bUpper vCs =
  let n = length vCs
      costs = DPV.fromList $ map (\x -> costFunc bLow x bUpper) vCs
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
      focSq b = NC.sumElements $ DPV.mapVector (**2) $ focFunc bLow vCs cdfs pdfs b
      foc = NC.sumElements $ DPV.mapVector focSq bs
      lowerBound = NC.sumElements $ DPV.mapVector (**2) $ lowerBoundFunc bLow vCs lowers
      upperBound = NC.sumElements $ DPV.mapVector (**2) $ upperBoundFunc bLow bUpper vCs
  in foc + fromIntegral granularity * lowerBound + fromIntegral granularity * upperBound

{-
  Impure (main) program goes here
-}
-- Minimization
main :: IO b
main = do
  let w = 0.52
  let reps = [0.5, 0.75]
  let lowers = lowerExt w reps
  let uppers = upperExt w reps
  let bUpper = upperBoundBidsFunc lowers uppers
  let granularity = 100
  let objective = objFunc granularity bUpper lowers uppers
  let l1 = lowerExt w reps !! 1
  let (s,_) = GSL.minimize GSL.NMSimplex2 1E-8 100000 (take 13 [1E-1,1E-1..]) objective (take 13 (l1 : [1E-2,1E-2..]))
  let bLow = head s
  putStr "Estimated lower bound on bids: "
  print bLow
  let cs = DPV.fromList $ drop 1 s
  let bs = DPV.toList $ NC.linspace 1000 (bLow, bUpper)
  CHART.plotPDF "polynomial.pdf" bs (costFunc bLow $ DPV.subVector 0 6 cs) "-" (costFunc bLow $ DPV.subVector 6 6 cs) "- "

{-
  Specification of tests goes here
-}
-- Test costFunc
testCostFunc :: HUNIT.Test
testCostFunc = HUNIT.TestCase (do
  let err = 1E-8
  let xs = [0.0,0.1..1.0]
  let expYs = map (\x -> x**2 - 0.5*x + 0.75) xs
  let ys = map (costFunc 0.5 (DPV.fromList [0.25,0.5,1.0])) xs
  let cmp = all (== True) $ zipWith (\x y -> abs (x-y) < err) expYs ys
  HUNIT.assertBool "Testing costFunc: " cmp)
-- Test derivCostFunc
testDerivCostFunc :: HUNIT.Test
testDerivCostFunc = HUNIT.TestCase (do
  let err = 1E-8
  let xs = [0.0,0.1..1.0]
  let expYs = map (\x -> 2*x - 0.5) xs
  let ys = map (derivCostFunc 0.5 (DPV.fromList [0.25,0.5,1.0])) xs
  let cmp = all (== True) $ zipWith (\x y -> abs (x-y) < err) expYs ys
  HUNIT.assertBool "Testing derivCostFunc: " cmp)

tests :: HUNIT.Test
tests = HUNIT.TestList [HUNIT.TestLabel "testCostFunc" testCostFunc,
                        HUNIT.TestLabel "testDerivCostFunc" testDerivCostFunc]
