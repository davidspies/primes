import Test.QuickCheck
import Test.Hspec

import Lib

main :: IO ()
main = hspec $
  describe "rollWheel" $
    it "Should return primes" $ property $ \(Small k) -> sized $ \s ->
      return $ take s (rollWheel k) === take s primes
