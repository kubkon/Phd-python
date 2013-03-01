import Control.Applicative
import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.Container as NC
import qualified Data.CSV as CSV
import qualified Bajari as B

-- FoC vector function
focFunc :: 
  NC.Vector Double     -- vector of upper extremities
  -> Double            -- independent variable
  -> NC.Vector Double  -- vector of inputs
  -> NC.Vector Double  -- vector of derivatives
focFunc uppers t ys =
  let n = NC.dim ys
      probV = NC.zipVectorWith (-) uppers ys
      rsV = NC.mapVector (\x -> 1 / (t - x)) ys
      constV = NC.constant (sum (NC.toList rsV) / (fromIntegral n - 1)) n
  in NC.mul probV $ NC.sub constV rsV

-- Forward shooting method
forwardShooting ::
  Double                                              -- upper bound on bids
  -> (NC.Vector Double -> NC.Matrix Double)           -- list of lower extremities
  -> Double                                           -- desired error
  -> (Double -> NC.Vector Double)                     -- grid function
  -> Double                                           -- lower bound on estimate
  -> Double                                           -- upper bound on estimate
  -> IO (Double, NC.Matrix Double)                    -- tuple of estimate and matrix of solutions
forwardShooting bUpper odeSolver err ts low high = do
  let guess = 0.5 * (low + high)
  let s = odeSolver $ ts guess
  if high - low < err
    then return (guess, s)
    else do
      let bids = NC.toList $ ts guess
      let costs = map NC.toList $ NC.toColumns s
      let inits = map head costs
      let condition1 = concat $ zipWith (\l c -> map (\x -> l <= x && x <= bUpper) c) inits costs
      let condition2 = concatMap (zipWith (>=) bids) costs
      if and (condition1 ++ condition2)
        then forwardShooting bUpper odeSolver err ts low guess
        else forwardShooting bUpper odeSolver err ts guess high

-- Main
main :: IO ()
main = do
  let w = 0.5
  let reps = [0.25, 0.75]
  let lowers = B.lowerExt w reps
  let uppers = B.upperExt w reps
  let bUpper = B.upperBoundBidsFunc lowers uppers
  let ts low = NC.linspace 1000 (low, bUpper)
  let xdot = focFunc (NC.fromList uppers)
  let odeSolver = ODE.odeSolveV ODE.RKf45 0.01 1E-5 1E-5 xdot (NC.fromList lowers)
  let low = lowers !! 1
  let high = bUpper
  let err = 1E-6
  (bLow, s) <- forwardShooting bUpper odeSolver err ts low high
  let bids = NC.toList $ ts bLow
  let values = bids : map NC.toList (NC.toColumns s)
  let struct = replicate (1 + NC.rows s) []
  let csvVals = getZipList $ foldr ((\acc x -> (:) <$> acc <*> x) . ZipList) (ZipList struct) values
  let writeToCSV = CSV.genCsvFile . map (map show)
  let csvString = writeToCSV csvVals
  let filePath = "ode.out"
  writeFile filePath csvString
