-- Requires , QuickCheck >= 2.0, lazysmallcheck >= 0.3
module Test.Stream where
import Data.Stream
import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary)
import Test.LazySmallCheck (Serial, series, cons2)

instance Arbitrary a => Arbitrary (Stream a) where
    arbitrary = liftM2 Cons arbitrary arbitrary
  
instance CoArbitrary a => CoArbitrary (Stream a) where
    coarbitrary xs gen = do
        n <- arbitrary
        coarbitrary (take (abs n) xs) gen
  
instance Serial a => Serial (Stream a) where
    series = cons2 Cons
        