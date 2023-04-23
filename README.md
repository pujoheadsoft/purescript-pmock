# purescript-pmock

pmock is mocking library for PureScript

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
      m = mock $ (any :: Param String) :> 2023 :> false
      
    fun m "Title1"  2023 `shouldEqual` false -- OK
    fun m "Another" 2023 `shouldEqual` false -- OK
    fun m ""        2023 `shouldEqual` false -- OK
```
`any` allows the built-in matcher to accept arbitrary values.
When used, it must be annotated with a type annotation to make the type explicit.

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
```
This verifies that the function has never been called with any value combination.

## Custom Matcher
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
The matcher takes two arguments, the first defined as `forall a. (a -> Boolean)`. The second argument is the message to be displayed if the first function returns false.

This matcher can also be used for verify.
```haskell
import Prelude

import Test.PMock (Param, VerifyMatchType(..), any, fun, matcher, mock, verify, (:>))
import Test.Spec (Spec, it)

spec :: Spec Unit
spec = do
  it "any match example" do
    let
      m = mock $ "Title" :> (any :: Param Int) :> false

      _ = fun m "Title" 2020
      _ = fun m "Title" 2001

    verify m $ AllMatch $ "Title" :> matcher (\v -> v > 2000) "> 2000"
```
If multiple calls to a function are expected, use `AllMatch` to verify that all calls were made with the expected values.
By default, verify will succeed if any one of the multiple calls is called with the expected value.
If you do not use any matcher, such as `mock $ "Name" :> 100`, you would not need to use `AllMatch` because you would not verify anything but the exact matching input.

## Multi Mock
