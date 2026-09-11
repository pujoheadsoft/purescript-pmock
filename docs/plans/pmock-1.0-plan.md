# PMock 1.0 計画

## 目的

PMock 0.10.2を安定した基準点とし、MockCatで得られた設計上の改善と実利用からのフィードバックをPureScriptへ取り込む。

1.0では、単に既存APIの名前を変更するのではなく、次の利用者向けモデルを明確にする。

- 戻り値だけを差し替えるなら、記録しない純粋な`stub`を使う。
- 呼び出しを検証するなら、記録する`mock`を使う。
- `stub`と`mock`が返した値は、そのまま対象コードへ渡せる関数または`Effect`である。
- Mockの期待値は原則として定義時に宣言し、スコープ終了時に自動検証する。
- 探索的なテストや細かな診断のため、事後検証も残す。
- Matcherを使う場合、引数型へテスト都合の`Eq`や`Show`を要求しない。

## 背景と判断

### 0.10.2で解決済みのこと

- 引数なしの`Effect a`を、その実行時点で記録できる。
- `mockSequence`で呼び出しごとに異なる結果を返せる。
- 引数の最大数が固定ではなくなった。
- 実装モジュールとテストモジュールの責務を対応させた。
- modern Spagoへ移行し、`--pedantic-packages`をCIで実行している。

これらは1.0で作り直さず、回帰を防ぐ既存機能として扱う。

### 1.0で受け入れる破壊的変更

0系はベータ版であり、公開利用も限定的であるため、APIの一貫性を優先して破壊的変更を許容する。ただし、動作の破壊とAPI表現の変更は区別する。0.10.2の振る舞いを意図せず失わないことをテストで保証する。

### Mockを直接関数として返すことの代償

現在の`Mock fun params`は、関数本体と型付きの呼び出し記録を同じ値に保持する。このため事後検証の対象がMockであることを型で保証できる。一方、直接関数APIでは、返り値の型は通常の`a -> b`になり、その型だけからMockか通常の関数かを区別できない。

1.0では、この代償を次のように管理する。

- 生成した関数とRecorderの対応はJavaScriptの`WeakMap`へ登録する。
- 事後検証へ通常の関数を渡した場合は、明確な実行時エラーにする。
- 定義時期待値では、Mock生成時にRecorderを直接登録するため、関数からRecorderを逆引きしない。
- 型付きハンドルが必要な利用者向けに、低水準の明示的APIを内部または別モジュールとして残す。
- 関数をラップする変換やFFI境界では同一性が変わり得ることを文書化する。

これはMockCatと同じ問題領域だが、Haskellの`StableName`を模倣するのではなく、JavaScriptランタイムに自然な`WeakMap`を使う。関数を直接扱う利便性を公開APIで優先し、型安全なRecorder操作は実装境界に閉じ込める。

## 目標APIのたたき台

最終的な名前と演算子優先順位は型検証用のスパイク後に確定する。以下は利用感の目標であり、この文面のまま実装することを先に固定しない。

### Stub

```purescript
let find = stub $ "Aja" :> 1977

find "Aja" `shouldEqual` 1977
```

- `stub`は純粋で、呼び出しを記録しない。
- 検証が不要な依存差し替えの標準手段にする。
- 不正な引数に対する即時失敗と診断メッセージは維持する。

### 定義時期待値を持つMock

```purescript
withMocks do
  save <- mock (any :> pure unit)
    `expects` called once `with` expectedProgress

  runUseCase { save }
```

- `mock`は関数そのものを返し、`fun`による取り出しを不要にする。
- `withMocks`を抜けるときに登録済み期待値をすべて検証する。
- 対象コードや検証が失敗した場合も、元の例外を失わずに後処理できる設計にする。
- 複数の期待値を一つのMockへ登録できるようにする。

PureScriptで例外時も確実に検証するにはスコープの型が重要である。最初のスパイクでは、`Aff`の`bracket`相当を使うAPIと、テストフレームワークへ依存しない`Effect`中心のAPIを比較する。自動検証の信頼性を落としてまで過度に一般化しない。

### 事後検証

```purescript
save <- mock $ any :> pure unit

runUseCase { save }

save `shouldHaveBeenCalled` once `with` expectedProgress
save `shouldHaveBeenCalled` times 1
```

- 引数ありの回数検証に加え、全引数を対象とした総呼び出し回数を検証できる。
- `times 1`、`once`、`never`、`atLeast n`、`atMost n`を共通語彙にする。
- `hasBeenCalledTimes`など多数の長い派生関数は、1.0で整理または互換モジュールへ隔離する。
- 事後検証は補助的なAPIとし、READMEでは定義時期待値を先に紹介する。

### 引数なしEffect

```purescript
withMocks do
  load <- mock (pure emptyProgress)
    `expects` run once

  progress <- load
```

- `Effect a`を取り出した時点ではなく、実行時に記録する0.10.2の意味論を維持する。
- 引数のない関数呼び出しと区別するため、文章上は`called`ではなく`run`を候補にする。
- 事後検証でも総実行回数を引数なしで指定できるようにする。

### 名前付け

```purescript
save <- mock (label "saveProgress") $ any :> pure unit
```

`namedMock`、`namedMockSequence`のように組合せごとの関数を増やす方式は避け、`label`のような直交する修飾子へ統一する案を優先する。

## 1.0の必須範囲

### 1. `stub`と`mock`の責務分離

- `stub`は記録なしの純粋な関数構築を担当する。
- `mock`は記録と検証を担当する。
- 現在の`mockFun`相当の用途は`stub`または直接関数を返す`mock`へ吸収する。
- Multi Mockと逐次応答の意味論は維持する。

### 2. 直接関数API

- `mock`の結果を`fun`なしで対象の関数型Portへ渡せるようにする。
- JavaScript FFIに関数からRecorderを引く`WeakMap`レジストリを実装する。
- 登録、取得、未登録、スコープ破棄を独立してテストする。
- `Effect a`もJavaScript上の関数である点を利用しつつ、実行記録のタイミングを回帰テストする。
- レジストリは`WeakMap`を使い、Mock関数を不要に生存させない。

### 3. 定義時期待値と検証スコープ

- `Expectation params`と、複数期待値を構築できるDSLを定義する。
- 呼び出し有無、回数、引数、厳密順序、部分順序を既存Verifierから再利用する。
- スコープ終了時の自動検証を実装する。
- ネストしたスコープ、Mockなしのスコープ、複数Mock、対象コード失敗時をテストする。
- グローバルな「最後に作ったMock」へ依存せず、生成時にRecorderをスコープへ明示登録する。

### 4. Matcherと値の分離

現在の`Param a`は値、Matcher、表示を一つに持ち、`Eq (Param a)`と`Show (Param a)`を通して照合している。この構造がCustom Matcherにも`Eq a`と`Show a`を要求する原因になっている。

次の方向へ内部表現を分離する。

- 期待引数: `matches :: actual -> Boolean`と期待値の説明を持つ。
- 実引数: 呼び出し履歴として値を保持する。
- 照合: `Eq`インスタンスではなく専用の`matchesArgs`を使う。
- 表示: Matcherの説明と、任意の実値レンダラーを使う。
- 生値を期待値にした場合だけ`Eq a`と`Show a`を要求する。
- `any`と述語Matcherは`Eq a`を要求しない。
- `Show a`がない実値の既定表示は、型安全なプレースホルダーにする。

候補API:

```purescript
matcherBy
  { matches: \actual -> ...
  , renderExpected: "valid learning session"
  , renderActual: Just renderSession
  }
```

`unsafeCoerce`で説明文字列を値に見せている現在の実装は、この再設計で除去する。

### 5. エラーメッセージの統一

- Stubの不正引数、Mockの不正引数、事後検証、自動検証で語彙と字下げを統一する。
- Mock名、期待値、実値、回数、順序上の差分を一貫して表示する。
- 表示文言は専用の`MessageSpec`で固定する。
- 一つの失敗で十分な情報が得られる場合、内部型やレジストリの詳細を利用者へ露出しない。

### 6. 移行ガイドと公開面の整理

- `Test.PMock`から推奨APIだけをexportする。
- 高度な型やBuilder実装は`Internal`へ移すか、非推奨の低水準モジュールとして明示する。
- 0.10.2からの機械的な書き換え表を用意する。
- `fun m`の除去、`mockFun`から`stub`への変更、検証語彙の対応を例示する。
- README英語版と日本語版を同じ章構成にする。
- Repository Port、`a -> Effect Unit`、`Effect a`、複数回呼び出しを実例に含める。

## 1.1以降の候補

### 複数Mockをまたぐ呼び出し順

```text
loadProgress -> saveProgress -> reportOutput
```

1.0の検証スコープへ、全Mock共通の単調増加シーケンス番号とラベルを記録できる土台を用意する。ただし、公開する順序DSLは実装詳細への結合を強めるため、1.0必須にはしない。

1.1候補として次を検証する。

- 複数Mockをまたぐ厳密順序と部分順序。
- 同じMockを複数回含む順序。
- 引数Matcherを含むイベント列。
- 非同期処理での「開始順」「Effect実行順」の定義。
- 失敗時にMock名を並べた読みやすいタイムライン表示。

### Casesと逐次応答DSLの統合

`mockSequence`という別コンストラクタを維持するか、`cases`、`onCase`、`returnsInOrder`のような構成可能な定義へ統合するかを1.0実装中に評価する。APIを大きくするだけなら1.1へ送る。

## 実装の進め方

すべてTDDで進める。テストを失敗させ、最小実装で通し、リファクタリングする順序を崩さない。

### Phase 0: 基準点を固定する

1. `v0.10.2`で全テストと`spago build --pedantic-packages`が通ることを記録する。
2. 現在のテストを0.10.2の振る舞いを示すcharacterization testとして扱う。
3. 既存テストを都合よく書き換えて回帰を隠さない。破壊的API変更でコンパイル不能になる箇所は、移行コミットを独立させる。
4. 1機能1コミットを基本とし、各コミットを単独でテスト可能にする。

### Phase 1: Matcher内部表現を先に直す

1. `Eq`なしの型を`any`でStub/Mock化する失敗テストを書く。
2. `Eq`なしの型を述語Matcherで照合する失敗テストを書く。
3. `Show`なしの場合と`renderActual`指定時のエラー表示テストを書く。
4. 期待引数と実引数を分離し、`Eq args`/`Show args`への一括制約をなくす。
5. 生値、`any`、`notEqual`、`and`、`or`の既存意味論を回帰確認する。

この順番にする理由は、Stub、Mock、期待値DSLのすべてが同じ引数照合モデルを使うためである。公開APIを先に増やしてから基盤を交換する二重作業を避ける。

### Phase 2: Stubを分離する

1. 単一ケース、複数引数、Multi Stub、不正引数のテストを書く。
2. 記録を一切生成しない純粋Builderを実装する。
3. `stub`が通常の関数型Portへ直接適合することをコンパイルで保証する。
4. `Effect`を返す関数と、`Effect a`そのものをStub化した場合の意味論を固定する。
5. 状態を持つ逐次応答は純粋なStubへ含めず、`mockSequence`の回帰テストで保証する。

### Phase 3: 直接関数MockとRecorderレジストリ

1. FFIレジストリ単体テストを先に書く。
2. `mock`が直接関数を返す利用例をテストする。
3. 事後検証が同じ関数からRecorderを取得できることをテストする。
4. 通常関数、ラップ後の関数、登録解除後の関数に対する診断をテストする。
5. 引数なし`Effect`の記録タイミングと失敗時の記録を回帰確認する。

### Phase 4: 期待値DSLと自動検証

1. 回数のみ、引数付き回数、未呼び出し、順序の期待値テストを書く。
2. 一つのMockに複数期待値を登録するテストを書く。
3. 複数Mockを同じスコープで検証するテストを書く。
4. 正常終了、対象コードの失敗、検証失敗、両方の失敗をテストする。
5. スコープを実装し、定義時期待値をREADMEの推奨経路にする。

### Phase 5: 公開APIを確定する

型スパイクで、次を実際にコンパイルさせて比較する。

- `expects`と`with`の演算子優先順位。
- `called once`、`called (times 2)`、`run once`の読みやすさ。
- 総呼び出し回数と引数付き回数を一つの語彙で表せるか。
- `label`を引数として渡す位置。
- Multi Mockと逐次応答を別関数にするか、定義DSLへ統合するか。
- 型推論エラーが利用者に理解可能か。

API名はREADME風のコンパイルテストを通してから確定する。型として成立しない見た目を先に約束しない。

### Phase 6: 移行とドキュメント

1. READMEのQuick Startを`stub`と定義時期待値の二本立てにする。
2. 事後検証を補助的手段として説明する。
3. 0.10.2から1.0.0への移行ガイドを追加する。
4. CHANGELOGへBreaking Changesを明記する。
5. Pursuitに不要な内部モジュールが露出していないことを確認する。

### Phase 7: リリース検証

次をすべて満たしてから1.0.0をtag付けする。

- `spago build --pedantic-packages`
- `spago test --pedantic-packages`
- `spago docs`
- README内のコード例をコンパイルするテスト
- 0.10.2の意味論に対するcharacterization test
- Registry manifestのpreviewまたはpublish前検証
- クリーンな作業ツリーからの再ビルド

## テスト構成

ソースとテストのモジュール対応を維持する。

```text
src/Test/PMock/Stub.purs             test/Test/PMock/StubSpec.purs
src/Test/PMock/Registry.purs         test/Test/PMock/RegistrySpec.purs
src/Test/PMock/Expectation.purs      test/Test/PMock/ExpectationSpec.purs
src/Test/PMock/Scope.purs            test/Test/PMock/ScopeSpec.purs
src/Test/PMock/Param.purs            test/Test/PMock/ParamSpec.purs
src/Test/PMock/Message.purs          test/Test/PMock/MessageSpec.purs
```

公開APIの結合テストは`test/Test/PMockSpec.purs`へ置き、内部の網羅的な型クラス検証と分ける。引数数ごとの重複テストは作らず、再帰の基底、再帰ステップ、十分に長い代表例で無制限arityを保証する。

## 互換性方針

1.0ではソース互換性を必須にしないが、移行経路は必ず提供する。

候補は次の二つであり、Phase 5で決める。

1. `Test.PMock.Legacy`を一時提供し、0.10.2の`Mock`/`fun`/検証APIを残す。
2. 互換層を持たず、移行ガイドと明確なコンパイルエラーを優先する。

互換層が新実装を二重化する場合は採用しない。薄いadapterとして実現でき、保守期限を明示できる場合だけ提供する。提供するなら1.x中にdeprecatedとし、2.0で削除する。

## 明示的に避けること

- 型だけでは保証できない関数同一性を、保証できるかのように説明しない。
- `unsafeCoerce`を公開APIの整合性維持に使わない。
- グローバルな「直前のMock」に期待値を結び付けない。
- Stubに記録コストや検証用の状態を持たせない。
- Custom Matcherのためにプロダクション型へ`Eq`/`Show`を要求しない。
- 複数Mock順序検証を、意味論が曖昧なまま1.0へ詰め込まない。
- 既存テストの削除や期待値の弱体化で移行を通さない。

## 1.0完了条件

- `stub`と`mock`の違いを一文で説明でき、型と動作も一致している。
- 生成したMockを`fun`なしで関数型Portへ渡せる。
- 定義時期待値が正常終了時と失敗時に確実に扱われる。
- 総呼び出し回数を引数なしで検証できる。
- Custom Matcher使用時に引数型の`Eq`/`Show`が不要である。
- 引数なし`Effect`、Multi Mock、逐次応答、無制限arityが回帰していない。
- 通常関数を事後検証した場合のエラーが明確である。
- 英語・日本語README、移行ガイド、CHANGELOG、Pursuit API docsが揃っている。
- pedantic build/testとCIが成功している。

## 最初に着手する作業

最初の実装タスクは、直接関数APIではなくMatcher内部表現の再設計とする。

理由は、`stub`、直接関数Mock、定義時期待値のすべてが引数照合を共有するためである。ここを先に安定させれば、その後の公開API実装を同じ基盤の上で進められる。最初のRed testは、`Eq`も`Show`も持たない型を述語MatcherでMock化し、呼び出しと検証が成功する例にする。
