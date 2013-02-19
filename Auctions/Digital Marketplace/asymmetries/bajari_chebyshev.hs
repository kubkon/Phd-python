import qualified Math.Polynomial.Chebyshev as MPC
import qualified Data.Random.Distribution.Uniform as U
import qualified Numeric.GSL.Minimization as GSL
import qualified Graphics.Rendering.Chart.Simple as CHART

-- Slice helper
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Translate to Chebyshev's domain
translate :: Double -> Double -> Double -> Double
translate a b x = (2.0 * x - (a + b)) / (b - a)

-- Chebyshev's polynomial of 1st kind
chebyshev1st :: [Double] -> Double -> Double
chebyshev1st cs x = sum $ zipWith (*) cs $ take n $ MPC.evalTs x
  where n = length cs

-- Chebyshev's polynomial of 2nd kind
chebyshev2nd :: [Double] -> Double -> Double
chebyshev2nd cs x = sum $ zipWith (*) cs $ take n $ MPC.evalUs x
  where n = length cs

-- Uniform CDF
uniformCDF a b x = U.realUniformCDF a b x
-- Uniform PDF
uniformPDF :: RealFrac a => a -> a -> a -> Double
uniformPDF a b x
  | b < a = uniformPDF b a x
  | x <= a || x >= b = 0
  | otherwise = realToFrac (1 / (b-a))

-- Cost function
costFunc :: (Double, Double) -> [Double] -> Double -> Double
costFunc (l, u) cs b = chebyshev1st cs $ translate l u b

-- Derivative of cost function
derivCostFunc :: (Double, Double) -> [Double] -> Double -> Double
--derivCostFunc (l, u) (c:cs) b = (2 / (u - l)) * chebyshevSum
derivCostFunc (l,u) (c:cs) b = chebyshev2nd cs $ translate l u b
  where chebyshevSum = chebyshev2nd cs $ translate l u b

-- FoC function
focFunc :: (Double, Double) -> Double -> [Double] -> [Double] -> Double -> Double
focFunc bounds upper cs ocs b = 1 + sums
  where sums = (b - cost) * (derivCostFunc bounds ocs b) / (upper - oCost)
        cost = costFunc bounds cs b
        oCost = costFunc bounds ocs b

-- Objective function
objFunc :: (Double, Double) -> (Double, Double) -> (Double, Double) -> [Double] -> Double
objFunc bounds@(bLow,bUp) (l1, l2) (u1, u2) cs = 5*sum1 + 5*sum2 + 5000*c11 + 2000*c12 + 5000*c21 + 2000*c22
  where cs1 = slice 0 14 cs
        cs2 = slice 15 29 cs
        bs = [bLow,bLow+0.01..bUp]
        sum1 = sum $ map (^2) $ map (focFunc bounds u2 cs1 cs2) bs
        sum2 = sum $ map (^2) $ map (focFunc bounds u1 cs2 cs1) bs
        c11 = (l1 - costFunc bounds cs1 bLow)^2
        c12 = (bUp - costFunc bounds cs1 bUp)^2
        c21 = (l2 - costFunc bounds cs2 bLow)^2
        c22 = (bUp - costFunc bounds cs2 bUp)^2

main = do
  let b1 = 0.5
  let b2 = 0.875
  let bounds = (b1, b2)
  let lowers = (0.0625, 0.1875)
  let uppers = (0.8125, 0.9375)
  let (s,p) = GSL.minimize GSL.NMSimplex2 1E-8 100000 (take 30 [0.001,0.001..]) (objFunc bounds lowers uppers) (take 30 [0.01,0.01..])
  CHART.plotPDF "auctions.pdf" [b1,b1+0.01..b2] (costFunc bounds (slice 0 14 s)) "- " (costFunc bounds (slice 15 29 s)) "-"

