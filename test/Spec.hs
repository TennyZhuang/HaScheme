import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Repl
import Interpreter.Define
import Interpreter.Operand

newtype SmallInt = SmallInt { unWrap :: Integer } deriving (Show)

instance Arbitrary SmallInt where
  arbitrary  = SmallInt `liftM` choose (1, 20)

prop_number :: Positive Integer -> Property
prop_number (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay $ show n
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == fromInteger n)

prop_add :: Positive Double -> Positive Double -> Property
prop_add (Positive x) (Positive y) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(+ ", show x, " ", show y, ")"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == x + y)

prop_mul :: Positive Double -> Positive Double -> Property
prop_mul (Positive x) (Positive y) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(* ", show x, " ", show y, ")"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == x * y)

prop_bool_not :: Property
prop_bool_not = monadicIO $ do
  value <- run $ evalAnyWay "(not #f)"
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapBool value
  assert res

prop_bool_and :: Property
prop_bool_and = monadicIO $ do
  value <- run $ evalAnyWay "(and #t #f)"
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapBool value
  assert $ not res

prop_bool_or :: Property
prop_bool_or = monadicIO $ do
  value <- run $ evalAnyWay "(or #t #f)"
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapBool value
  assert res

prop_higherOrder :: SmallInt -> Property
prop_higherOrder n = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(((lambda (a) ((lambda (b) (b b)) (lambda (b) (a (lambda (c) ((b b) c)))))) (lambda (f) (lambda (n) (if (and (< (- n 1) 0.1) (< (- 1 n) 0.1)) 1 (* n (f (- n 1))))))) ", show (unWrap n), ")"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == fromInteger (factorial (unWrap n))) where
    factorial n = if n < 2 then 1 else n * factorial (n - 1)

prop_scope :: Positive Integer -> Property
prop_scope (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(begin (define x ", show n, ") (define (f x) (set! x 4)) (f 10) x))"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == fromInteger n)

prop_while :: Positive Integer -> Property
prop_while (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(begin (define x 0) (while (< x ", show n,") (set! x (+ x 1))) x)"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == fromInteger n)

prop_ordinary_function :: Positive Integer -> Property
prop_ordinary_function (Positive n) = monadicIO $ do
  value <- run $ evalAnyWay $ concat ["(begin (define (f x) (* x x)) (f ", show n,"))"]
  res <- liftIO . unwrapIOThrows . liftThrows $ unwrapNumber value
  assert (res == fromInteger (n * n))

main = do
  quickCheck prop_number
  quickCheck prop_add
  quickCheck prop_mul
  quickCheck prop_bool_not
  quickCheck prop_bool_and
  quickCheck prop_bool_or
  quickCheck prop_higherOrder
  quickCheck prop_scope
  quickCheck prop_while
  quickCheck prop_ordinary_function
