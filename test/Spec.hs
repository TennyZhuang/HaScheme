import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class (liftIO)
import Repl
import Interpreter.Define
import Interpreter.Operand

eps :: Double
eps = 1e-5

prop_eval :: Positive Integer -> Property
prop_eval (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay (show n)
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (abs (res - (fromInteger n)) < eps)

main = quickCheck prop_eval
