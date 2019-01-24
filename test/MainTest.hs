
import Test.Tasty

import TestFile
import TestFdr

main :: IO ()
main = defaultMain allTests

allTests = testGroup " Unit tests " [test_File, test_Fdr]
