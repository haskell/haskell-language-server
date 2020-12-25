{- |
The Eval plugin evaluates code in comments.

This is mainly useful to:

* quickly evaluate small code fragments

* test and document functions

Every line of code to be evaluated is introduced by __>>>__.

A quick calculation:

>>> 2**4.5/pi
7.202530529256849

A little test for the `double` function:

>>> double 11
22

You execute a test by clicking on the /Evaluate/ or /Refresh/ (if the test has been run previously) code lens that appears above it.

All tests in the same comment are executed together.

Tests can appear in all kind of comments: plain or Haddock (forward of backwards), single line or multiple line.

Both plain Haskell and Literate Haskell (Bird-style only) source files are supported.

A test can be composed of multiple lines:

>>> "AB" ++ "CD"
>>> "CD" ++ "AB"
"ABCD"
"CDAB"

In general, a test is a sequence of:

* imports

* directives

* statements

* expressions

* properties

in no particular order, with every line introduced by __>>>__ (or __prop>__ in the case of properties).

= Test Components

== Imports

>>> import Data.List
>>> import GHC.TypeNats

From any package in scope but currently NOT from modules in the same source directory.

== Language Extensions

>>> :set -XScopedTypeVariables -XStandaloneDeriving -XDataKinds -XTypeOperators -XExplicitNamespaces

=== Statements and Declarations

Function declarations (optionally introduced by /let/):

>>> let tuple x = (x,x)
>>> let one=1;two=2
>>> triple x = (x,x,x)

Any other declaration:

>>> data TertiumDatur = Truly | Falsely | Other deriving Show
>>> class Display a where display :: a -> String
>>> instance Display TertiumDatur where display = show

Definitions are available to following tests in the __same__ comment.

If you want definitions to be available to all tests in the module, define a setup section:

@
-- $setup
-- >>> eleven = 11
@

/eleven/ is now available to any test.

== Type and Kind directives

>>> :type Truly
Truly :: TertiumDatur

>>> :kind TertiumDatur
TertiumDatur :: *

>>> :type 3
3 :: forall p. Num p => p

>>> :type +d 3
3 :: Integer

>>> type N = 1
>>> type M = 40
>>> :kind! N + M + 1
N + M + 1 :: Nat
= 42

== Expressions

>>> tuple 2
>>> triple 3
>>> display Other
(2,2)
(3,3,3)
"Other"

IO expressions can also be evaluated but their output to stdout/stderr is NOT captured:

>>> print "foo"
()

== Properties

prop> \(l::[Int]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.

= Haddock vs Plain Comments

There is a conceptual difference between Haddock and plain comments.

Haddock comments constitute the external module's documentation while plain comments are internal documentation meant to explain how the code works (api vs implementation).

This conceptual difference is reflected in the way tests results are refreshed by the Eval plugin.

Tests in plain comments are refreshed by overwriting the previous result.

On the contrary, when tests in Haddock comments are refreshed their current result is compared with the previous one and differences are displayed.

Say for example that we have defined a test on the `thrice` function, defined as:

>>> thrice = (*3)

If by mistake at a later time we change its definition to:

>>> thrice = (*2)

When we refresh its test we get a warning:

>>> thrice 11
WAS 33
NOW 22

== Tip: Multiline Output

By default, the output of every expression is returned as a single line.

This is a problem if you want, for example, to pretty print a value (in this case using the <https://hackage.haskell.org/package/pretty-simple pretty-simple> package):

>>> import Text.Pretty.Simple
>>> pShowNoColor [1..3]
"[ 1\n, 2\n, 3\n]"

We could try to print the pretty-print output, but stdout is not captured so we get just a ():

>>> print $ pShowNoColor [1..7]
()

To display it properly, we can exploit the fact that the output of an error is displayed as a multi-line text:

>>> import qualified Data.Text.Lazy as TL
>>> import Text.Pretty.Simple
>>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
>>> prettyPrint [1..3]
[ 1
, 2
, 3
]

= Differences with doctest

Though the Eval plugin functionality is quite similar to that of <https://hackage.haskell.org/package/doctest doctest>, some doctest features are not supported.

== Capturing Stdout

Only the value of the expression is spliced in, not its output:

>>> print "foo"
()

== Pattern Matching

The arbitrary content matcher __...__ is unsupported.

== Missing lambda abstractions in property tests

Variables are not automatically introduced:

prop> reverse (reverse l) == (l::[Int])
Variable not in scope: l :: [Int]
Variable not in scope: l :: [Int]

This works:

prop> \(l::[Int]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.

== Multiline Expressions

@
 >>> :{
  let
    x = 1
    y = 2
  in x + y + multiline
 :}
@
-}
module Ide.Plugin.Eval.Tutorial (
    double,
) where

{- | Double a number

An example of a simple test suite for a function (all tests are executed together).

>>> double 1
2

>>> double 11
22

>>> double 22
44
-}
double :: Num a => a -> a
double n = n * 2

{- ORMOLU_DISABLE -}
-- $setup
-- >>> x = 11;
-- >>> y = 22

-- >>> (x,y)
-- (11,22)

