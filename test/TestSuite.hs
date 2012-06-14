import Test.Framework

import qualified Control.OperationalTransformation.Text.Tests

main :: IO ()
main = defaultMain
  [ Control.OperationalTransformation.Text.Tests.tests
  ]