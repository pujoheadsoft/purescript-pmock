# purescript-pmock

[![Latest release](https://img.shields.io/github/release/pujoheadsoft/purescript-pmock.svg)](https://github.com/pujoheadsoft/purescript-pmock/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-pmock/badge)](https://pursuit.purescript.org/packages/purescript-pmock)
[![CI](https://github.com/pujoheadsoft/purescript-pmock/actions/workflows/ci.yml/badge.svg)](https://github.com/pujoheadsoft/purescript-pmock/actions/workflows/ci.yml)
[![License](https://img.shields.io/github/license/pujoheadsoft/purescript-pmock.svg)](LICENSE)

pmockはPureScriptのモックライブラリです。

[English README](README.md)

PMock 0.10以前の使い方は、[旧版の日本語README](docs/README-v0.10-ja.md)を参照してください。

PMockでは、入力と戻り値を指定するだけで、差し替える依存と同じ関数型を持つStubを作ることができます。  
まずStubで十分かを考え、呼び出しの検証や逐次応答が必要な場合だけMockを使用します。  
複数ケースやMatcherを型付きDSLで定義できます。  
StubとMockでは同じ入力DSLを使用できますが、複数の`onCase`が一致した場合の選択規則は異なります。  
単一の値を返すだけの手書きStubと比べ、受け付ける入力を明示し、想定外の入力では差分を含むメッセージを表示できます。  
Mockの検証に失敗した場合も、原因を調べるための詳しいメッセージを表示します。

```text
Stub first. Verification when needed.
```

PMockは、引数やレコードを通して注入される関数または`Effect`を差し替えるライブラリです。  
モジュールの`import`を横取りしたり、依存を自動的に書き換えたりするものではありません。  
プロダクションコードは、差し替える依存を関数やレコードとして受け取る必要があります。

| | 入力の照合 | 呼び出し履歴 |
| --- | --- | --- |
| Stub | 入力を照合して戻り値を選ぶ | 記録しない |
| Mock | 同じ入力DSLを使うが、定義形式によって複数ケースの選択規則が異なる | 記録し、一つのMock内の回数、引数、順序を検証できる |

## 特徴

- プロダクションコードにPMock専用の型を持ち込まず、同じ関数型または`Effect`型で依存を差し替えられます。
- 型の付いた入力ケース、`any`、述語Matcherを使用できます。
- 一つのMockに対する呼び出し回数、引数、順序を検証できます。
- 呼び出しごとに戻り値を変える逐次応答を定義できます。
- 期待した引数で呼ばれていない場合は、表示文字列の先頭が最も一致する呼び出し、文字列やレコードの差分、呼び出し履歴を表示します。
- 回数や順序の検証では、期待値と実際の値を検証内容に応じた形式で表示します。
- 関数の引数の数に固定上限はありません。

## ドキュメント

- [Quick Start](#quick-start)
- [Stubを作る](#stubを作る)
- [Mockを作る](#mockを作る)
- [エラーメッセージ](#エラーメッセージ)
- [利用上の制約](#利用上の制約)
- [purescript-specで実行時エラーを捕捉する](#purescript-specで実行時エラーを捕捉する)
- [0.10からの移行](#010からの移行)

## インストール

[Spago](https://github.com/purescript/spago)でテスト用の依存パッケージとしてインストールします。  
Quick Startのように`purescript-spec`から直接importする場合は、`spec`も追加してください。

```sh
spago install pmock --test-deps
spago install spec --test-deps
```

APIリファレンスは[Pursuit](https://pursuit.purescript.org/packages/purescript-pmock)で参照できます。

## 対応環境

CIでは、次の組み合わせでビルドとテストを実行しています。

| PureScript | package set |
| --- | --- |
| 0.15.15 | 80.9.0 |
| 0.15.12 | 46.0.2 |

[spago.yaml](spago.yaml)には、公開マニフェスト上で許容する依存パッケージのバージョン範囲を記載しています。

## Quick Start

例えば、プロダクションコードがレコードで依存を受け取る場合は、次のようにテストできます。

```purescript
module Test.AlbumSpec (spec) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Test.PMock (mock, once, shouldBeCalled, stub, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type AlbumDependencies =
  { findYear :: String -> Int
  , notify :: Effect Unit
  }

describeAlbum :: AlbumDependencies -> String -> Effect String
describeAlbum dependencies title = do
  dependencies.notify
  pure $ title <> " (" <> show (dependencies.findYear title) <> ")"

spec :: Spec Unit
spec = describe "album" do
  it "describes an album with stubs" do
    let
      dependencies =
        { findYear: stub $ "Aja" :> 1977
        , notify: stub (pure unit :: Effect Unit)
        }

    result <- liftEffect $ describeAlbum dependencies "Aja"

    result `shouldEqual` "Aja (1977)"

  it "notifies when describing an album" do
    notify <- mock (pure unit :: Effect Unit)
    let
      dependencies =
        { findYear: stub $ "Aja" :> 1977
        , notify
        }

    result <- liftEffect $ describeAlbum dependencies "Aja"

    result `shouldEqual` "Aja (1977)"
    notify `shouldBeCalled` once
```

最初のテストでは、依存する関数と`Effect`をStubに差し替えています。  
通知されたこと自体が仕様になる二つ目のテストだけ、`notify`をMockにして検証を追加しています。

StubとMockは、差し替える依存と同じ関数型または`Effect`型を持つため、プロダクションコードにPMock専用の型を持ち込む必要はありません。  
これは型の互換性についての説明です。  
Mock関数は、テスト用の内部状態に呼び出しを記録するため、参照透過な実装ではありません。  
また、Stubも定義されていない入力では同期例外を投げます。

これ以降の短いコードブロックは、説明する箇所だけを抜き出したもので、`import`は一部省略しています。

## Stubを作る

`stub`に受け付ける引数と戻り値を渡すと、関数を作ることができます。  
引数と戻り値は`:>`で区切ります。  
最後の値が戻り値です。

```purescript
import Test.PMock (stub, (:>))

let findYear = stub $ "Aja" :> 1977

findYear "Aja" -- 1977
```

定義されていない入力でStubを呼び出すと、その場で同期例外になります。  
受け付ける入力は、追加の`onCase`、`matcher`、`any`で指定してください。

複数の引数も同じように`:>`で区切ります。

```purescript
let find = stub $ "Aja" :> 1977 :> true :> "found"

find "Aja" 1977 true -- "found"
```

`stub`が返すものは特別なハンドルではなく、差し替える依存と同じ型の関数です。  
プロダクションコードがレコードで依存を受け取る場合も、そのまま渡せます。

```purescript
type AlbumRepository =
  { findYear :: String -> Int
  }

let repository =
      { findYear: stub $ "Aja" :> 1977
      }
```

プロダクションコードが依存を関数やレコードとして受け取る設計であれば、PMockを使うための専用の型を追加する必要はありません。

### 複数の入力を定義する

引数によって戻り値を変える場合は、`onCase`を並べます。

```purescript
import Test.PMock (onCase, stub, (:>))

let
  findYear = stub do
    onCase $ "Aja" :> 1977
    onCase $ "Gaucho" :> 1980

findYear "Aja"    -- 1977
findYear "Gaucho" -- 1980
```

Stubでは、複数の`onCase`を上から順に照合し、常に最初に一致した定義を使用します。  
例えば`any`を具体的な値より前に置くと、後ろの定義には到達しません。

### Matcher

任意の引数を受け取る場合は`any`を使用します。

```purescript
import Test.PMock (any, stub, (:>))

let findYear = stub $ any @String :> 1977
```

`matcher`を使うと、述語で引数を指定できます。

```purescript
import Test.PMock (matcher, stub, (:>))

let positive = matcher (_ > 0) "a positive number"
let classify = stub $ positive :> "positive"
```

入力を値で指定する場合、その型には`Eq`と`Show`が必要です。  
用意できない型には`matcher`または`any`を使用できます。

述語Matcherと`any`では、対象の型に`Eq`や`Show`は必要ありません。  
エラー時に実際の値も表示したい場合は、`matcherBy`で表示方法を指定できます。

```purescript
import Data.Maybe (Maybe(..))
import Test.PMock (matcherBy)

let positive = matcherBy
      { matches: \value -> value > 0
      , renderExpected: "a positive number"
      , renderActual: Just \value -> "number " <> show value
      }
```

使用できるMatcherは次のとおりです。

| 指定 | 意味 |
| --- | --- |
| `any` | 任意の値に一致する |
| `matcher predicate description` | 述語が`true`を返す値に一致する |
| `matcher_ predicate` | 説明を省略して述語を指定する |
| `matcherBy specification` | 述語、期待値の説明、実値の表示方法を指定する |
| ``a `or` b`` | どちらかに一致する |
| ``a `and` b`` | 両方に一致する |
| `notEqual value` | 指定した値と異なる値に一致する |

```purescript
import Test.PMock (notEqual, or, stub, (:>))

let classify = stub $ (1 `or` 2 `or` 3) :> "small"
let exceptFive = stub $ notEqual 5 :> "not five"
```

### Effectを返す関数

戻り値には`Effect`も指定できます。

```purescript
import Effect (Effect)
import Test.PMock (stub, (:>))

let save = stub $ "progress" :> (pure unit :: Effect Unit)

save "progress"
```

引数のない`Effect a`も、そのままStubにできます。

```purescript
let load = stub (pure 42 :: Effect Int)

result <- load
```

Stubは呼び出しを記録しません。  
戻り値を差し替えるだけなら、Stubを使用してください。

## Mockを作る

呼び出しを検証する場合や逐次応答が必要な場合は、`mock`を使用します。  
`mock`は呼び出しを記録する状態を用意するため、`Effect`を実行できるモナド内で生成し、`<-`で取り出します。  
`Effect`だけでなく、`Aff`や`Spec`の中でも直接生成できます。  
取り出した値は、差し替える依存と同じ関数型または`Effect`型なので、そのままプロダクションコードへ渡せます。

```purescript
findYear <- mock $ "Aja" :> 1977

findYear "Aja" `shouldEqual` 1977
findYear `shouldBeCalled` "Aja"
```

### 期待値を先に宣言する

期待値をMockの定義と一緒に宣言する場合は、`withMock`と`expects`を使用します。  
`expects`は必ず`withMock`の内側で使用します。  
`withMock`を抜けるときに、登録された期待値が検証されます。

```purescript
import Effect.Class (liftEffect)
import Test.PMock
  ( called
  , expects
  , mock
  , once
  , with
  , withMock
  , (:>)
  )

result <- liftEffect $ withMock do
  findYear <- mock ("Aja" :> 1977)
    `expects` (called once `with` "Aja")

  pure $ findYear "Aja"
```

`withMock`が囲むことができるのは、同期的に完了する`Effect`です。  
`Aff`などの非同期処理全体を`withMock`で直接囲むことはできません。  
Mockの戻り値として`Aff`を指定できることとは別なので注意してください。

`Aff`の処理では、処理の実行後に`shouldBeCalled`で検証します。
この例を使用する場合は、プロジェクトへ`aff`を追加してください。

```sh
spago install aff --test-deps
```

```purescript
import Effect.Aff (Aff)
import Test.PMock (any, mock, once, shouldBeCalled, with, (:>))

findYear <- mock $
  any @String :> (pure 1977 :: Aff Int)

year <- findYear "Aja"

year `shouldEqual` 1977
findYear `shouldBeCalled` (once `with` "Aja")
```

一つのMockへ複数の期待値を指定できます。

```purescript
liftEffect $ withMock do
  save <- mock (any @String :> (pure unit :: Effect Unit))
    `expects` do
      called once `with` "article-1"
      called never `with` "missing"

  save "article-1"
```

定義時には次の期待値を指定できます。

| 指定 | 意味 |
| --- | --- |
| `called count` | Mock全体の呼び出し回数を検証する |
| ``called count `with` arguments`` | 指定した引数での呼び出し回数を検証する |
| `calledInOrder arguments` | 呼び出し全体が、指定した引数と順序に一致することを検証する |
| `calledInPartialOrder arguments` | 指定した引数が、その順序で呼ばれたことを検証する |

### 実行後に検証する

呼び出した後で検証することもできます。

```purescript
output <- mock $ any @String :> (pure unit :: Effect Unit)

output "first"
output "second"

output `shouldBeCalled` times 2
output `shouldBeCalled` (once `with` "first")
```

Matcherは、Mockの定義と事後検証にも使用できます。  
次の`Command`型には`Eq`と`Show`のインスタンスがありませんが、述語Matcherだけで定義と検証ができます。

```purescript
data Command = Save String

let nonEmptyCommand =
      matcher
        (\(Save value) -> value /= "")
        "a non-empty Save"
      :: Param Command

handle <- mock $ nonEmptyCommand :> unit

handle (Save "article-1") `shouldEqual` unit
handle `shouldBeCalled` (once `with` nonEmptyCommand)
```

`shouldBeCalled`には、`mock`が直接返した関数を渡してください。  
事後検証では関数の同一性を使ってMockを識別するため、ラップした関数を渡すことはできません。

```purescript
mockFn <- mock $ any @String :> 1977
let wrapped input = mockFn input

wrapped "Aja" `shouldEqual` 1977

mockFn `shouldBeCalled` once
-- wrapped `shouldBeCalled` once -- 検証できません
```

元のMock関数を保持しない場合は、ラップする前に`withMock`と`expects`で期待値を宣言してください。

総呼び出し回数だけを検証する場合、引数を指定する必要はありません。  
`with`で引数を指定した場合は、その引数に一致した呼び出しだけを数えます。

回数には次の指定が使用できます。

| 指定 | 意味 |
| --- | --- |
| `never` | 0回 |
| `once` | 1回 |
| `times n` | ちょうどn回 |
| `atLeast n` | n回以上 |
| `atMost n` | n回以下 |
| `greaterThan n` | n回より多い |
| `lessThan n` | n回より少ない |

事後検証では、引数をそのまま渡すと、その引数で1回以上呼ばれたことを検証します。

```purescript
findYear `shouldBeCalled` "Aja"
findYear `shouldBeCalled` calledWith "Aja"
findYear `shouldBeCalled` anything
```

最初の2行は同じ意味です。  
`anything`は、引数を問わず1回以上呼ばれたことを検証します。

複数の引数は、Mockの定義と同じように`:>`で区切り、一つの引数列として指定します。

```purescript
save <- mock $ any @String :> any @Int :> unit

save "Aja" 1977
save "Gaucho" 1980

save `shouldBeCalled` (once `with` ("Aja" :> 1977))
save `shouldBeCalled` calledWith ("Gaucho" :> 1980)
save `shouldBeCalled` inOrderWith
  [ "Aja" :> 1977
  , "Gaucho" :> 1980
  ]
```

呼び出し順を検証する場合は、次のように書きます。

```purescript
output `shouldBeCalled` inOrderWith [ "first", "second" ]
output `shouldBeCalled` inPartialOrderWith [ "first", "last" ]
```

| 指定 | 意味 |
| --- | --- |
| `inOrderWith arguments` | すべての呼び出しが、指定した引数と順序に一致することを検証する |
| `inPartialOrderWith arguments` | 間に別の呼び出しがあっても、指定した順序が保たれていることを検証する |

### 引数なしEffectを検証する

引数付きのMockが`Effect`を返す場合は、引数を適用した時点で呼び出しが記録されます。  
返された`Effect`を実行するかどうかは、呼び出し回数に影響しません。

```purescript
save <- mock $ any @String :> (pure unit :: Effect Unit)

let action = save "progress"
save `shouldBeCalled` once

liftEffect action
save `shouldBeCalled` once
```

一方、引数のない`Effect a`は、作成時ではなく`Effect`の実行時に記録されます。

```purescript
load <- mock (pure 42 :: Effect Int)

load `shouldBeCalled` never
result <- load
load `shouldBeCalled` once
```

### 戻り値を順番に返す

同じ引数に対して`onCase`を複数指定すると、呼び出しごとに戻り値が変わります。  
最後の値まで到達した後は、最後の値を繰り返し返します。

```purescript
next <- mock do
  onCase $ unit :> 1
  onCase $ unit :> 2

next unit -- 1
next unit -- 2
next unit -- 2
```

逐次応答の現在位置は、同じ`onCase`の集合に一致する呼び出しで共有されます。  
例えば、次のMockは引数に関係なく、最初の呼び出しで`1`を返し、二回目以降は`2`を返します。

```purescript
next <- mock do
  onCase $ any @String :> 1
  onCase $ any @String :> 2

next "A" -- 1
next "B" -- 2
next "A" -- 2
```

具体値などによって一致する`onCase`の集合が異なる場合は、それぞれの逐次応答が独立して進みます。

Mockでは、入力に一致するすべての`onCase`が連続応答の候補になります。  
`any`と具体値のようにMatcherが重複する場合、具体値の呼び出しには両方が一致し、定義順に戻り値が選ばれます。  
常に最初に一致した値を返すMulti Mockとして使う場合は、配列形式の`mock [ ... ]`を使用してください。

```purescript
firstMatch <- mock
  [ any @String :> 1
  , "A" :> 2
  ]

firstMatch "A" -- 1
firstMatch "A" -- 1
```

Stub、逐次応答を行うMock、Multi Mockの違いは次のとおりです。

| 定義方法 | 複数の定義が一致した場合 |
| --- | --- |
| `stub do onCase ...` | 上から最初に一致した定義を常に使用する |
| `mock do onCase ...` | 一致した定義を逐次応答として定義順に使用する |
| `mock [ ... ]` | 上から最初に一致した定義を常に使用する |

### 名前を付ける

複数のStubやMockを使用する場合は、`label`で名前を付けるとエラーを見つけやすくなります。

```purescript
let findYear = stub (label "findYear") ("Aja" :> 1977)

save <- mock (label "saveProgress")
  (any @String :> (pure unit :: Effect Unit))
```

## 利用上の制約

- `withMock`が囲めるのは同期的に完了する`Effect`だけなので、非同期処理では[実行後の検証](#実行後に検証する)を使用してください。
- `expects`は`withMock`の内側でのみ使用できます。
- ラップした関数は同じMockとして識別できないため、実行後の検証には`mock`が直接返した関数を渡してください。
- `:>`の右側にはあらかじめ決めた戻り値を指定するため、実引数から戻り値を計算するAnswer機能はなく、関数を指定した場合はその関数自体を戻り値として返します。
- `mockIt`が追加で捕捉するのは、テスト用のモナド値を組み立てる間の同期例外です。

詳しい説明は、それぞれの使用例を参照してください。

## エラーメッセージ

期待した引数で呼ばれていない場合は、表示文字列の先頭が期待値と最も一致する呼び出しと、呼び出し履歴（Call history）を表示します。  
文字列やレコードの値が異なる場合は、異なる箇所も表示します。  
回数や順序の検証では、期待値と実際の値を検証内容に応じた形式で表示します。  
`label`を指定していれば、どのStubまたはMockで失敗したかも確認できます。

例えば、`findYear`が`"hello world"`で呼ばれたことを検証した場合、次のようなメッセージになります。

```purescript
findYear <- mock (label "findYear") (any @String :> 1977)

findYear "goodbye" `shouldEqual` 1977
findYear "hello purescript" `shouldEqual` 1977

findYear `shouldBeCalled` "hello world"
```

```text
function `findYear` was not called with the expected arguments.

  Closest match:
    expected: "hello world"
     but got: "hello purescript"
                   ^^^^^^^^^^^

  Call history (2 calls):
              1. "goodbye"
    [Closest] 2. "hello purescript"
```

レコードの場合は、異なっているフィールド名と、そのフィールドの期待値、実際の値を表示します。

## purescript-specで実行時エラーを捕捉する

StubまたはMockとして作った関数は、期待していない引数で呼び出された場合にJavaScriptの同期例外を投げます。  
`purescript-spec`を使用している場合は、`it`の代わりに`mockIt`を使用すると、その例外を該当するテストの失敗として表示できます。

```purescript
import Test.PMock (stub, (:>))
import Test.PMock.Spec (mockIt)
import Test.Spec (Spec)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  mockIt "find an album year" \_ -> do
    let findYear = stub $ "Aja" :> 1977

    findYear "Gaucho" `shouldEqual` 1980
```

`mockIt`の型と引数の並びは`Test.Spec.it`と同じです。  
StubとMockのどちらを使うテストにも使用できます。

`mockIt`が捕捉するのは、テスト関数からテスト用のモナド値を組み立てる間に発生する同期例外です。  
`Aff`の非同期処理中や、bind後に実行される処理の例外を追加で捕捉するものではありません。  
それらの例外は、通常の`purescript-spec`の仕組みでテスト失敗として扱われます。

## 0.10からの移行

PMock 1.0では、StubとMockの役割を分け、PMock専用のハンドルを介さずにどちらも直接呼び出せるようにしました。  
主なAPIの対応は次のとおりです。

| 0.10 | 1.0 | 備考 |
| --- | --- | --- |
| `mockFun definition` | `stub definition` | 呼び出しを記録しない差し替えにはStubを使用する |
| `mock definition` | `mock definition` | 1.0では`Mock`ハンドルではなく関数を直接返す |
| `fun mock` | `mock`が返した関数 | `fun`による取り出しは不要 |
| `namedMockFun name definition` | `stub (label name) definition` | 名前は`label`で指定する |
| `namedMock name definition` | `mock (label name) definition` | 名前は`label`で指定する |
| `mockSequence definitions` | `mock do onCase ...` | 同じ引数の`onCase`を順番に返す |
| `namedMockSequence name definitions` | `mock (label name) do onCase ...` | `label`と`onCase`を組み合わせる |
| `mock [ definition1, definition2 ]` | `mock [ definition1, definition2 ]` | 従来のMulti Mockと同じく、最初に一致した定義を使用する |
| `verify mock arguments` | ``mock `shouldBeCalled` arguments`` | 指定した引数で1回以上呼ばれたことを検証する |
| ``mock `hasBeenCalledWith` arguments`` | ``mock `shouldBeCalled` arguments`` | 同上 |
| ``mock `hasNotBeenCalledWith` arguments`` | ``mock `shouldBeCalled` (never `with` arguments)`` | 指定した引数で呼ばれていないことを検証する |
| ``mock `hasBeenCalledTimes` n `with` arguments`` | ``mock `shouldBeCalled` (times n `with` arguments)`` | 指定した引数での回数を検証する |
| ``mock `hasBeenRunTimes` n`` | ``mock `shouldBeCalled` times n`` | 引数なし`Effect`の実行回数を検証する |
| ``mock `hasBeenCalledInOrder` arguments`` | ``mock `shouldBeCalled` inOrderWith arguments`` | 呼び出し全体の値と順序を検証する |
| ``mock `hasBeenCalledInPartialOrder` arguments`` | ``mock `shouldBeCalled` inPartialOrderWith arguments`` | 指定した呼び出しの順序を検証する |
| `GreaterThanEqual n` | `atLeast n` | n回以上 |
| `LessThanEqual n` | `atMost n` | n回以下 |
| `GreaterThan n` | `greaterThan n` | n回より多い |
| `LessThan n` | `lessThan n` | n回より少ない |
| `MatchAll matcher` | ``never `with` matcher (not <<< predicate) ...`` | 条件を満たさない呼び出しがないことを検証する |
| `showCalledParams mock` | 直接の後継なし | 検証エラーの呼び出し履歴（Call history）で呼び出しを確認する |
| `Test.PMockSpecs.mockIt` | `Test.PMock.Spec.mockIt` | 関数名は同じで、import先が変わる |

`Mock`、`CountVerifyMethod`、`VerifyMatchType`などの内部表現は、1.0の公開APIには含まれません。

PMock 0.10.2のドキュメントは削除していません。

- [PMock 0.10.2 日本語README](docs/README-v0.10-ja.md)
- [PMock 0.10.2 English README](docs/README-v0.10.md)

## License

[MIT License](LICENSE)
