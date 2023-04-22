# purescript-pmock

pmock is mocking library for PureScript

## Use mock function
The `mockfun` function can be used to create mock functions.
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
The `mockfun` function is passed arguments that are expected to be called, separated by `:>`.
The last value separated by a `:>` is the return value.
This is similar to a type declaration where types are separated by `->` and the last separated type is the return type.
(In the above example, a type definition is written in findArticle to clearly indicate the correspondence.)

## Verify function
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