import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class (liftIO)
import Repl
import Interpreter.Define
import Interpreter.Operand

eps :: Double
eps = 1e-5

prop_number :: Positive Integer -> Property
prop_number (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay $ show n
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (abs res - fromInteger n < eps)

prop_add :: Positive Double -> Positive Double -> Property
prop_add (Positive x) (Positive y) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(+ ", show x, " ", show y, ")"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (abs res - (x + y) < eps)

prop_mul :: Positive Double -> Positive Double -> Property
prop_mul (Positive x) (Positive y) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(* ", show x, " ", show y, ")"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (abs res - (x * y) < eps)

main = do
  quickCheck prop_number
  quickCheck prop_add
  quickCheck prop_mul
