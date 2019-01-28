
import Test.Tasty

import TestFile
import TestCorrection

main :: IO ()
main = defaultMain allTests

allTests = testGroup " Unit tests " [test_File, test_Correction]
