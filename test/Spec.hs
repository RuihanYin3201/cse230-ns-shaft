import           Shaft
import           Test.QuickCheck

main :: IO ()
main = do
    quickCheck prop_genBernoulliList
