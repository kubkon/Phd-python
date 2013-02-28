import qualified Numeric.GSL.ODE as ODE
import qualified Numeric.Container as NC
import qualified Graphics.Plot as GP
import qualified Bajari as B

-- FoC vector function
focFunc :: 
  NC.Vector Double     -- vector of upper extremities
  -> Double             -- independent variable
  -> NC.Vector Double  -- vector of inputs
  -> NC.Vector Double  -- vector of derivatives
focFunc uppers t ys =
  let n = NC.dim ys
      probV = NC.zipVectorWith (-) uppers ys
      rsV = NC.mapVector (\x -> 1 / (t - x)) ys
      constV = NC.constant (sum (NC.toList rsV) / (fromIntegral n - 1)) n
  in NC.mul probV $ NC.sub constV rsV

-- Main
main :: IO ()
main = do
  let w = 0.5
  let reps = [0.25, 0.75]
  let lowers = B.lowerExt w reps
  let uppers = B.upperExt w reps
  let bLow = 0.5178175311899669
  let bUpper = B.upperBoundBidsFunc lowers uppers
  let xdot = focFunc (NC.fromList uppers)
  let ts = NC.linspace 100 (bLow, bUpper)
  let s = ODE.odeSolveV ODE.RKf45 0.01 1E-8 1E-8 xdot (NC.fromList lowers) ts
  GP.mplot (ts : NC.toColumns s)
