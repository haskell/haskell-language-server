
module HaddockTests (tests) where

import           Development.IDE.Spans.Common
-- import Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests
  = testGroup "haddock"
      [ testCase "Num" $ checkHaddock
          (unlines
             [ "However, '(+)' and '(*)' are"
             , "customarily expected to define a ring and have the following properties:"
             , ""
             , "[__Associativity of (+)__]: @(x + y) + z@ = @x + (y + z)@"
             , "[__Commutativity of (+)__]: @x + y@ = @y + x@"
             , "[__@fromInteger 0@ is the additive identity__]: @x + fromInteger 0@ = @x@"
             ]
          )
          (unlines
             [ ""
             , ""
             , "However,  `(+)`  and  `(*)`  are"
             , "customarily expected to define a ring and have the following properties: "
             , "+ ****Associativity of (+)****: `(x + y) + z`  =  `x + (y + z)`"
             , "+ ****Commutativity of (+)****: `x + y`  =  `y + x`"
             , "+ ****`fromInteger 0`  is the additive identity****: `x + fromInteger 0`  =  `x`"
             ]
          )
      , testCase "unsafePerformIO" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  * Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  * Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "+ Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "+ Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      , testCase "ordered list" $ checkHaddock
          (unlines
             [ "may require"
             , "different precautions:"
             , ""
             , "  1. Use @{\\-\\# NOINLINE foo \\#-\\}@ as a pragma on any function @foo@"
             , "        that calls 'unsafePerformIO'.  If the call is inlined,"
             , "        the I\\/O may be performed more than once."
             , ""
             , "  2. Use the compiler flag @-fno-cse@ to prevent common sub-expression"
             , "        elimination being performed on the module."
             , ""
             ]
          )
          (unlines
             [ ""
             , ""
             , "may require"
             , "different precautions: "
             , "1. Use  `{-# NOINLINE foo #-}`  as a pragma on any function  `foo` "
             , "  that calls  `unsafePerformIO` .  If the call is inlined,"
             , "  the I/O may be performed more than once."
             , ""
             , "2. Use the compiler flag  `-fno-cse`  to prevent common sub-expression"
             , "  elimination being performed on the module."
             , ""
             ]
          )
      ]
  where
    checkHaddock s txt = spanDocToMarkdownForTest s @?= txt
