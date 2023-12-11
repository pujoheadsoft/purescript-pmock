# purescript-pmock

[![Latest release](http://img.shields.io/github/release/pujoheadsoft/purescript-pmock.svg)](https://github.com/pujoheadsoft/purescript-pmock/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-pmock/badge)](https://pursuit.purescript.org/packages/purescript-pmock)

pmock is mocking library for PureScript

[日本語版 README](https://github.com/pujoheadsoft/purescript-pmock/blob/master/README-ja.md)

## Installation

Install with [Spago](https://github.com/purescript/spago):
```
spago install pmock
```

## Use mock function
The `mockFun` function can be used to create mock functions.
```haskell
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Test.PMock (mockFun, (:>))

type Article = {title :: String, body :: String}

main :: Effect Unit
main = do
  let
    findArticle :: String -> Int -> Article
    findArticle = mockFun $ "Title" :> 2023 :> {title: "ArticleTitle", body: "ArticleBody"}
  logShow $ findArticle "Title" 2023 -- { body: "ArticleBody", title: "ArticleTitle" }
```
The `mockFun` function is passed arguments that are expected to be called, separated by `:>`.
The last value separated by a `:>` is the return value.
This is similar to a type declaration where types are separated by `->` and the last separated type is the return type.
(In the above example, a type definition is written in findArticle to clearly indicate the correspondence.)

## Verifying function calls
The Mock type can be used to verify the call.
In the first example, we used `mockFun` to create the mock function directly, but when using the `Mock` type, we use `mock`.
In this case, the mock function can be taken out of the Mock type by using `fun`.
```haskell
import Prelude

import Test.PMock (fun, mock, verify, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "verify example" do
    let
      m = mock $ "Title" :> 2023 :> false

    -- execute function
    fun m "Title" 2023 `shouldEqual` false

    -- verify
    verify m $ "Title" :> 2023
```
If the verify does not match, the following message is output.
<pre style="color: #D2706E">
Function was not called with expected arguments.
expected: "Another Title",2022
but was : "Title",2023
</pre>

## Naming a mock function
You can name your mock functions with the `namedMockFun` and `namedMock` functions.
This name is used for messages when a mock function fails to call or verify.
This is useful if you are using multiple mock functions and want to know which mock function failed to call.
```haskell
import Prelude

import Test.PMock (namedMockFun, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "named mock function test" do
    let
      f = namedMockFun "namedMock" $ "a" :> true :> 100
    100 `shouldEqual` f "b" true
```
<pre>
Error: function `namedMock` was not called with expected arguments.
  expected: "a",true
  but was : "b",true
</pre>

## Verifying the number of function calls
`verifyCount` can be used to verify the number of times a mock function has been called.
This allows you to verify that a function has not been called.
```haskell
import Prelude

import Test.PMock (mock, verifyCount, (:>))
import Test.Spec (Spec, it)

spec :: Spec Unit
spec = do
  it "verify count example" do
    let
      m = mock $ "Title" :> 2023 :> false

    -- verify count
    verifyCount m 0 $ "Title" :> 2023
```
If the verify count does not match, the following message is output.
<pre style="color: #D2706E">
✗ verify count example:

Function was not called the expected number of times.
expected: 1
but was : 0
</pre>

## Specify the verify count method
There are four validation methods that can be used

`GreaterThanEqual`

`LessThanEqual`

`GreaterThan`

`LessThan`

Here is an example of use
```haskell
import Prelude

import Test.PMock (CountVerifyMethod(..), fun, mock, verifyCount, (:>))
import Test.Spec (Spec, it)

spec :: Spec Unit
spec = do
  it "verify example" do
    let
      m = mock $ "Title" :> 2023 :> false
      
      _ = fun m "Title" 2023
      _ = fun m "Title" 2023
      _ = fun m "Title" 2023
    verifyCount m (GreaterThanEqual 3) $ "Title" :> 2023
```
If the verify count does not match, the following message is output.
<pre style="color: #D2706E">
Function was not called the expected number of times.
expected: >= 4
but was : 3
</pre>

## Verifying function call sequential order
### Strict call order valification
verifySequence` verifies that the functions were called in the specified order.

For all calls to the function, it is verified that the order and values are exactly the same.
```haskell
import Prelude

import Test.PMock (any, fun, mock, verifySequence, (:>))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "verify exactly sequential order" do
      let
        m = mock $ any :> unit
        -- function call 3 times.
        _ = fun m "a"
        _ = fun m "b"
        _ = fun m "c"

      -- verify OK
      verifySequence m [
        "a",
        "b",
        "c"
      ]
```
If the verification fails, you will know the order of the calls that failed to verification and the expected and actual values at that order.

For example, suppose the expected value in the above example is as follows.
```haskell
verifySequence m [
  "b",
  "c",
  "a"
]
```
Then the verification fails and the following message appears.
<pre style="color: #D2706E">
Function was not called with expected order.
expected 1st call: "b"
but was  1st call: "a"
expected 2nd call: "c"
but was  2nd call: "b"
expected 3rd call: "a"
but was  3rd call: "c"
</pre>
### Partial call order valification
`verifyPartiallySequence` verifies that the functions were called in a specific order.

Unlike `verifySequence`, it is not necessary that all calls match exactly. As long as the order matches, the verification succeeds. For example, the following will verify that the calls were made in the order `"a"`,`"c"`.
```haskell
import Prelude

import Test.PMock (any, fun, mock, verifyPartiallySequence, (:>))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "verify partially sequential order" do
      let
        m = mock $ any :> unit
        _ = fun m "a"
        _ = fun m "b"
        _ = fun m "c"

      verifyPartiallySequence m [
        "a",
        "c"
      ]
```
If the verification fails, the expected calling order and the actual calling order are all displayed.
For example, suppose in the above example the expected values are as follows.
```haskell
verifySequence m [
  "c",
  "a"
]
```
Then the verification fails and the following message appears.
<pre style="color: #D2706E" >
Function was not called with expected order.
expected order:
  "c"
  "a"
actual order:
  "a"
  "b"
  "c"
</pre>

## Matcher
Matchers allow flexibility in setting and verifying expected arguments.
By default, a matcher is used to verify that the values are identical, but you can specify a matcher that accepts arbitrary values, for example
```haskell
import Prelude

import Test.PMock (Param, any, fun, mock, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "any match example" do
    let
      m = mock $ any :> 2023 :> false
      
    fun m "Title1"  2023 `shouldEqual` false -- OK
    fun m "Another" 2023 `shouldEqual` false -- OK
    fun m ""        2023 `shouldEqual` false -- OK
```
`any` allows the built-in matcher to accept arbitrary values.

This matcher can also be used for verify.
```haskell
import Prelude

import Test.PMock (Param, any, mock, verifyCount, (:>))
import Test.Spec (Spec, it)

spec :: Spec Unit
spec = do
  it "any match example" do
    let
      m = mock $ "Title" :> 2023 :> false
      
    verifyCount m 0 $ (any :: Param String) :> (any :: Param Int)

    -- In PureScript v0.15.10 or later, you can write.
    -- verifyCount m 0 $ any @String :> any @Int
```
This verifies that the function has never been called with any value combination.
Annotated to make the type explicit.

### Custom Matcher
Can also define your own matchers by using the `matcher` function.
The following is an example of a matcher that allows integer values greater than 2020.
```haskell
import Prelude

import Test.PMock (fun, matcher, mock, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "any match example" do
    let
      m = mock $ "Title" :> matcher (\v -> v >= 2020) ">= 2020" :> false
      
    fun m "Title" 2020 `shouldEqual` false -- OK
    fun m "Title" 2021 `shouldEqual` false -- OK
    fun m "Title" 2022 `shouldEqual` false -- OK
    fun m "Title" 2023 `shouldEqual` false -- OK
```
The matcher takes two arguments, the first defined as `forall a. (a -> Boolean)`.

The second argument is the message to be displayed if the first function returns false.

This matcher can also be used for verify.
```haskell
import Prelude

import Test.PMock (Param, VerifyMatchType(..), any, fun, matcher, mock, verify, (:>))
import Test.Spec (Spec, it)

spec :: Spec Unit
spec = do
  it "any match example" do
    let
      m = mock $ "Title" :> any :> false

      _ = fun m "Title" 2020
      _ = fun m "Title" 2001

    verify m $ MatchAll $ "Title" :> matcher (\v -> v > 2000) "> 2000"
```
If multiple calls to a function are expected, use `MatchAll` to verify that all calls were made with the expected values.
By default, verify will succeed if any one of the multiple calls is called with the expected value.

If you do not use any matcher, such as `mock $ "Name" :> 100`, you would not need to use `MatchAll` because you would not verify anything but the exact matching input.

### Other Built-in Matchers
The `or` allows any of several values, as in the following example.
```haskell
import Prelude

import Test.PMock (fun, mock, or, verify, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "OR Matcher test" do
      let
        m = mock $ 1 `or` 2 `or` 3 :> "OK"

      fun m 1 `shouldEqual` "OK"
      fun m 2 `shouldEqual` "OK"
      fun m 3 `shouldEqual` "OK"

      verify m 1
      verify m 2
      verify m 3
```

`and` can return a value only if multiple conditions are met, as follows
```haskell
import Prelude

import Test.PMock (fun, matcher, mock, verify, (:>))
import Test.PMock.Param (and)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "AND Matcher test" do
      let
        m = mock $ matcher (0 < _) "0 < x" `and` matcher (_ < 3) "x < 3" :> "OK"

      fun m 1 `shouldEqual` "OK"
      fun m 2 `shouldEqual` "OK"

      verify m 1
      verify m 2
```
You can use `not` to invert the condition as follows.

If a value is specified, it will be accepted other that value.
```haskell
import Prelude

import Test.PMock (fun, mock, not, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "Not Matcher test" do
      let
        m = mock $ not 5 :> "OK"

      fun m 4 `shouldEqual` "OK"
      fun m 6 `shouldEqual` "OK"
```
It can be combined with other `Matchers`.
```haskell
import Test.PMock (fun, mock, not, or, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Example Spec" do
    it "Not Matcher test" do
      let
        m = mock $ not (4 `or` 5) :> "OK"

      fun m 3 `shouldEqual` "OK"
      fun m 6 `shouldEqual` "OK"
```

## Multi Mock
Sometimes you may want to change the value returned depending on the arguments passed.
In such cases, multimocking can be used.

Usage is simple, just pass an array of parameters to the mock function.
```haskell
import Prelude

import Test.PMock (fun, mock, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "multi mock example" do
    let
      m = mock $ [
        "Aja" :> 1977,
        "Gaucho" :> 1980,
        "The Royal Scam" :> 1976
      ]

    fun m "Aja" `shouldEqual` 1977
    fun m "Gaucho" `shouldEqual` 1980
    fun m "The Royal Scam" `shouldEqual` 1976

    verify m "Aja"
    verify m "Gaucho"
    verify m "The Royal Scam"
```

## About runtime errors
If the test is called with arguments other than those set, the test is aborted and the expected arguments and the arguments actually used in the call are printed as a message.
```haskell
import Prelude

import Test.PMock (fun, mock, (:>))
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "throw runtime error example" do
    let
      m = mock $ "Aja" :> 1977
    fun m "Asia" `shouldEqual` 1977
```
<pre style="color: #D2706E">
Error: Function was not called with expected arguments.
  expected: "Aja"
  but was : "Asia"
    at Module.error (file:///home/source/purescript-pmock/output/Effect.Exception/foreign.js:6:10)
    at Module.$$throw (file:///home/source/purescript-pmock/output/Effect.Exception/index.js:16:45)
    at error (file:///home/source/purescript-pmock/output/Test.PMock/index.js:208:71)
    at file:///home/source/purescript-pmock/output/Test.PMock/index.js:241:24
    at file:///home/source/purescript-pmock/output/Test.PMock/index.js:253:53
    at file:///home/source/purescript-pmock/output/Test.PMock/index.js:271:74
    at file:///home/source/purescript-pmock/output/Test.PMock/index.js:337:75
    at file:///home/source/purescript-pmock/output/Test.ExampleSpec/index.js:11:122
    at file:///home/source/purescript-pmock/output/Test.ExampleSpec/index.js:12:3
    at ModuleJob.run (node:internal/modules/esm/module_job:175:25)
[error] Tests failed: exit code: 1
</pre>
For simple tests, it may be easy to read the message and find the failed test, but if there are multiple mocks with similar argument settings, detection may be difficult.

In that case, if you are using `purescript-spec`, you can use `mockIt` instead of `it`.
This is a wrapper function for the existing it function, and replacing it with it will allow you to catch runtime errors.
```haskell
import Prelude

import Test.PMock (fun, mock, (:>))
import Test.PMockSpecs (mockIt)
import Test.Spec (Spec)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  mockIt "catch runtime error example" \_ -> do
    let
      m = mock $ "Aja" :> 1977
    fun m "Asia" `shouldEqual` 1977
```
<pre style="color: #D2706E">
✗ catch runtime error example:

Error: Function was not called with expected arguments.
expected: "Aja"
but was : "Asia"
</pre>
The `Test.PMockSpecs` module defines `it` as an alias for `mockIt`, so use this one if you prefer.
```haskell
import Prelude

import Test.PMock (fun, mock, (:>))
import Test.PMockSpecs (it)
import Test.Spec (Spec)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  it "catch runtime error example" \_ -> do
    let
      m = mock $ "Aja" :> 1977
    fun m "Asia" `shouldEqual` 1977
```

## Mock type annotation.
The definition of the `Mock` type is

`data Mock fun params = Mock fun (Verifier params)`

where the first type parameter matches the definition of the function to mock.

The next type parameter represents the type of parameters to use for verify, each wrapped in a `Param` and concatenated with the operator `#>`.
```haskell
m :: Mock (String -> Int -> Boolean) (Param String #> Param Int)
m = mock $ "" :> 100 :> true
```


## Constraints
* Only instances of eq and show are currently allowed as mock arguments.
* The number of supported arguments is limited to 9. If you want to handle more than this number of arguments, define an instance of `MockBuilder`.
