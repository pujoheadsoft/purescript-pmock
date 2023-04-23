# purescript-pmock

pmock is mocking library for PureScript

## mock関数を使う
`mockFun` を使うとmock関数を生成することができます。
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
`mockFun` 関数には、呼び出されることを期待する引数を `:>` で区切って渡します。
最後に `:>` で区切られた値が戻り値となります。

これは、型宣言において型を `->` で区切り、最後に区切られた型が戻り値の型になるのと似ています
(上記の例では、対応関係を明確に示すために、findArticleに型定義を記述しています)。

## 呼び出しの検証
`Mock` 型を使うと呼び出しを検証することができるようになります。

最初の例では `mockFun` を使って直接モック関数を作っていましたが、`Mock`型を使う場合は `mock` を使います。

この場合、`fun` で `Mock` 型からモック関数を取り出すことができます。

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
verifyに失敗した場合、以下のようなメッセージが出力されます。
<pre style="color: #D2706E">
Function was not called with expected arguments.
expected: "Another Title",2022
but was : "Title",2023
</pre>
## 呼び出し回数の検証
`verifyCount` を使うと、モック関数が呼び出された回数を検証することができます。

これにより、ある関数が呼び出されていないことなどを確認することができます。
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
回数が一致しない場合、以下のようなメッセージが出力されます。
<pre style="color: #D2706E">
✗ verify count example:

Function was not called the expected number of times.
expected: 1
but was : 0
</pre>

## 回数を検証する方法を指定する
通常の完全一致とは別に、以下の4つの検証方法が使えます。

`GreaterThanEqual`

`LessThanEqual`

`GreaterThan`

`LessThan`

以下は使用例です。
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
回数が一致しない場合は以下のようなメッセージが出力されます。
<pre style="color: #D2706E">
Function was not called the expected number of times.
expected: >= 4
but was : 3
</pre>

## Matcher
Matcherを使うと、期待される引数の設定や検証を柔軟に行うことができます。

デフォルトでは、値が同一であることを検証するMatcherが使用されますが、例えば、任意の値を受け入れるマッチャーを指定することができます。
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
`any`は、組み込み型のMatcherで、任意の値を受け取れるようにするものです。
使用する場合は、型を明示するために型アノテーションを付ける必要があります。

Matcherは、`verify`にも使用できます。
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
この例は、関数がどのような値の組み合わせでも呼ばれていないことを確認する例です。

## Custom Matcher
`matcher` 関数を使って独自のMatcherを定義することができます。

以下は、2020以上の整数値を許容するMatcherの例です。
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
`matcher` は2つの引数をとり，第1引数は `forall a. (a -> Boolean)` という定義になっています．

第2引数は、第1引数の関数がfalseを返した場合に表示されるメッセージです。

この `matcher` は `verify` にも使用できます。
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

    verify m $ MatchAll $ "Title" :> matcher (\v -> v > 2000) "> 2000"
```
mock関数に対して複数回の呼び出しが予想される場合、`MatchAll` を使用することで、「すべて」の呼び出しが期待される値で行われたかどうかを検証することができます。

デフォルトでは、複数回の呼び出しのうち1つでも期待通りの値で呼び出されていれば、verifyは成功します。

もし、`mock $ "Name" :> 100` のようにMatcherを使用しない場合は、完全に一致する入力以外は検証しないでしょうから、`MatchAll` を使用する必要はないでしょう。

## Multi Mock
引数によって、返す値を変えたいこともあります。
そんな時に使えるのが Multi Mock です。

使い方は簡単で、`mock` 関数にパラメーターの配列を渡すだけです。
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

## 実行時エラーについて
期待していない引数で関数が呼び出された場合、テストは中止され、期待される引数と実際に呼び出しに使用された引数がメッセージとして出力されます。
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
<pre>
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
シンプルなテストであればメッセージを読んで失敗したテストを見つけることは容易かもしれませんが、似たような引数の設定を行なっているモックが複数存在する場合、発見が困難になるかもしれません。

そのような場合、`purescript-spec`を使用している場合は、`it`の代わりに`mockIt`を使用することができます。

これは既存の `it` 関数のラッパー関数で、これに置き換えることで実行時エラーを捕捉することができるようになります。
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
<pre>
✗ catch runtime error example:

Error: Function was not called with expected arguments.
expected: "Aja"
but was : "Asia"
</pre>
`Test.PMockSpecs` モジュールには `mockIt` のエイリアスとして `it` が定義されているので、お好みでこちらをご利用ください。
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

## Constraints
* 現在、mockの引数として使用できるのはeqとshowのインスタンスのみです。
* サポートされている引数の数は有限で、9個に制限されています。この数以上の引数を扱いたい場合は、`MockBuilder`のインスタンスを定義してください。