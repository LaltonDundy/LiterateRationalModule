import Fractions
import System.Random    (randomIO)
import Data.Bits 

--------------------------------
--       function to keep ints in certain range to prevent stackoverflow
--
format = (.&.) 8191

--------------------------------
assert _ True  = return ()
assert n False = error $ "Test failed: " ++ ( show n)

test = do
       {

       numerator   <- randomIO :: IO Int;
       denominator <- randomIO :: IO Int;
       f1          <- return.fracSimplify $ Frac (format numerator) (format denominator);

       numerator   <- randomIO :: IO Int;
       denominator <- randomIO :: IO Int;
       f2          <- return.fracSimplify $ Frac (format numerator) (format denominator);

       numerator   <- randomIO :: IO Int;
       denominator <- randomIO :: IO Int;
       f3          <- return.fracSimplify $ Frac (format numerator) (format denominator);

       assert 1  $ f1 + f2 + f3   == f3 + f2 + f1;
       assert 2  $ f1 * f2 * f3   == f3 * f2 * f1;
       assert 3  $ f1 + Frac 0 0  == f1;
       assert 4  $ f1 * Frac 0 0  == Frac 0 0;
       assert 5  $ f1 * Frac 1 1  == f1;
       assert 6  $ f1 * (f2 + f3) == (f1 * f2) + (f1 * f3);
       assert 7  $ f1 + (f2 + f3) == (f1 + f2) + f3;
       assert 8  $ f1 * (f2 * f3) == (f1 * f2) * f3;

       }

main = do {sequence $ map (\x -> test) [1..100]; print "success!";}
