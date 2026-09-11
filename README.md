# purescript-pmock

[![Latest release](https://img.shields.io/github/release/pujoheadsoft/purescript-pmock.svg)](https://github.com/pujoheadsoft/purescript-pmock/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-pmock/badge)](https://pursuit.purescript.org/packages/purescript-pmock)
[![CI](https://github.com/pujoheadsoft/purescript-pmock/actions/workflows/ci.yml/badge.svg)](https://github.com/pujoheadsoft/purescript-pmock/actions/workflows/ci.yml)
[![License](https://img.shields.io/github/license/pujoheadsoft/purescript-pmock.svg)](LICENSE)

pmock is a mocking library for PureScript.

[日本語版 README](README-ja.md)

For PMock 0.10 and earlier, see the [0.10 English README](docs/README-v0.10.md).

PMock creates stubs from expected inputs and return values. The result has the
same function type as the dependency it replaces. Start with a stub, and use a
mock only when call verification or sequential responses are needed.
Define multiple cases and matchers with a typed DSL. Stubs and mocks accept the
same input DSL, but use different selection rules when multiple `onCase`
entries match. Compared with a hand-written stub that simply returns one value,
a PMock stub states its accepted inputs and can report differences for
unexpected inputs. Detailed messages also help diagnose mock verification
failures.

```text
Stub first. Verification when needed.
```

PMock replaces functions or `Effect` values that are injected through
arguments or records. It does not intercept module imports or rewrite
dependencies automatically. Production code must receive the dependency as a
function or record field so that it can be replaced.

| | Input matching | Call history |
| --- | --- | --- |
| Stub | Matches inputs to select a return value | Not recorded |
| Mock | Uses the same input DSL, but multiple-case selection depends on the definition form | Recorded, allowing counts, arguments, and order to be verified within one mock |

## Features

- Replace a dependency with the same function or `Effect` type without adding PMock-specific types to production code.
- Define typed input cases with values, `any`, or predicate matchers.
- Verify call counts, arguments, and order within one mock.
- Return successive values across calls.
- See the call whose rendered text shares the longest prefix with the expectation, string and record differences, and call history when no call matches the expected arguments.
- See expected and actual values in a form appropriate to count and order verification.
- Use functions with no fixed argument-count limit.

## Documentation

- [Quick Start](#quick-start)
- [Creating a stub](#creating-a-stub)
- [Creating a mock](#creating-a-mock)
- [Failure messages](#failure-messages)
- [Important constraints](#important-constraints)
- [Catching runtime errors with purescript-spec](#catching-runtime-errors-with-purescript-spec)
- [Migrating from 0.10](#migrating-from-010)

## Installation

Install PMock as a test dependency with
[Spago](https://github.com/purescript/spago). Also add `spec` when importing
`purescript-spec` directly, as in the Quick Start.

```sh
spago install pmock --test-deps
spago install spec --test-deps
```

The API reference is available on
[Pursuit](https://pursuit.purescript.org/packages/purescript-pmock).

## Supported environments

CI builds and tests the following combinations:

| PureScript | package set |
| --- | --- |
| 0.15.15 | 80.9.0 |
| 0.15.12 | 46.0.2 |

[spago.yaml](spago.yaml) lists the dependency ranges allowed by the published
manifest; not every combination within those ranges is tested by CI.

## Quick Start

When production code receives dependencies in a record, a complete test module
can look like this:

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

The first test replaces both dependencies with stubs. Only the second test,
where notification itself is part of the behavior, replaces `notify` with a
mock and verifies the call.

Stubs and mocks have the same function or `Effect` types as the dependencies
they replace, so production code does not need PMock-specific types. This is a
statement about type compatibility: a mock function records calls in internal
test state and is not referentially transparent. A stub also throws a
synchronous exception for an input that has not been defined.

The shorter code blocks below show only the relevant part of a test and omit
some imports.

## Creating a stub

Pass the accepted arguments and return value to `stub`. Separate them with
`:>`; the final value is the return value.

```purescript
import Test.PMock (stub, (:>))

let findYear = stub $ "Aja" :> 1977

findYear "Aja" -- 1977
```

Calling a stub with an input that has not been defined throws a synchronous
exception immediately. Use additional `onCase` entries, `matcher`, or `any` to
specify the accepted inputs.

Separate multiple arguments with `:>` in the same way.

```purescript
let find = stub $ "Aja" :> 1977 :> true :> "found"

find "Aja" 1977 true -- "found"
```

`stub` returns a function with the type of the dependency rather than a
special handle. It can be placed directly in a record expected by production
code.

```purescript
type AlbumRepository =
  { findYear :: String -> Int
  }

let repository =
      { findYear: stub $ "Aja" :> 1977
      }
```

When production code receives dependencies as functions or records, no
PMock-specific type or interface needs to be introduced.

### Defining multiple inputs

Use `onCase` when the return value depends on the arguments.

```purescript
import Test.PMock (onCase, stub, (:>))

let
  findYear = stub do
    onCase $ "Aja" :> 1977
    onCase $ "Gaucho" :> 1980

findYear "Aja"    -- 1977
findYear "Gaucho" -- 1980
```

For a stub, `onCase` entries are checked from top to bottom and the first match
is always used. For example, placing `any` before a specific value makes the
later case unreachable.

### Matchers

Use `any` to accept any argument.

```purescript
import Test.PMock (any, stub, (:>))

let findYear = stub $ any @String :> 1977
```

Use `matcher` to match an argument with a predicate.

```purescript
import Test.PMock (matcher, stub, (:>))

let positive = matcher (_ > 0) "a positive number"
let classify = stub $ positive :> "positive"
```

When an input is specified as a value, its type must have `Eq` and `Show`
instances. Use `matcher` or `any` for types where those instances are not
available.

Predicate matchers and `any` do not require `Eq` or `Show` instances for the
argument type. Use `matcherBy` to provide an actual-value renderer for error
messages when needed.

```purescript
import Data.Maybe (Maybe(..))
import Test.PMock (matcherBy)

let positive = matcherBy
      { matches: \value -> value > 0
      , renderExpected: "a positive number"
      , renderActual: Just \value -> "number " <> show value
      }
```

The available matchers are:

| Specification | Meaning |
| --- | --- |
| `any` | Matches any value |
| `matcher predicate description` | Matches values for which the predicate returns `true` |
| `matcher_ predicate` | Specifies a predicate without a custom description |
| `matcherBy specification` | Specifies the predicate, expected description, and actual-value renderer |
| ``a `or` b`` | Matches either side |
| ``a `and` b`` | Matches both sides |
| `notEqual value` | Matches values other than the specified value |

```purescript
import Test.PMock (notEqual, or, stub, (:>))

let classify = stub $ (1 `or` 2 `or` 3) :> "small"
let exceptFive = stub $ notEqual 5 :> "not five"
```

### Effectful functions

An `Effect` can be used as a return value.

```purescript
import Effect (Effect)
import Test.PMock (stub, (:>))

let save = stub $ "progress" :> (pure unit :: Effect Unit)

save "progress"
```

An argument-free `Effect a` can also be stubbed directly.

```purescript
let load = stub (pure 42 :: Effect Int)

result <- load
```

Stubs do not record calls. Use a stub when only the returned behavior needs to
be replaced.

## Creating a mock

Use `mock` for call verification or sequential responses. Creating a mock is
effectful because PMock allocates state for call recording, so create it in a
monad that can run `Effect` and bind it with `<-`. It can be created directly
in `Effect`, `Aff`, or `Spec`. The resulting value has the same function or
`Effect` type as the dependency and can be passed directly to production code.

```purescript
findYear <- mock $ "Aja" :> 1977

findYear "Aja" `shouldEqual` 1977
findYear `shouldBeCalled` "Aja"
```

### Declaring expectations first

There are two ways to verify a mock: declare expectations before execution, or
verify calls after execution. Use `withMock` and `expects` to declare
expectations first. Registered expectations are checked automatically when
`withMock` exits. `expects` must be used inside `withMock`.

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

`withMock` scopes an `Effect` that completes synchronously. It cannot directly
wrap a complete asynchronous computation such as `Aff`. This is separate from
using an `Aff` value as the return value of a mock.

For an `Aff` computation, verify the mock with `shouldBeCalled` after the
computation has run:

Add `aff` to the project when using this example:

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

Multiple expectations can be declared for one mock.

```purescript
liftEffect $ withMock do
  save <- mock (any @String :> (pure unit :: Effect Unit))
    `expects` do
      called once `with` "article-1"
      called never `with` "missing"

  save "article-1"
```

The following expectations can be declared during setup:

| Specification | Meaning |
| --- | --- |
| `called count` | Verifies the total number of calls to the mock |
| ``called count `with` arguments`` | Verifies the number of calls matching the arguments |
| `calledInOrder arguments` | Verifies that the complete call history matches the arguments and order |
| `calledInPartialOrder arguments` | Verifies that the arguments were called in the specified order |

### Verifying after execution

Calls can also be verified after the code under test has run.

```purescript
output <- mock $ any @String :> (pure unit :: Effect Unit)

output "first"
output "second"

output `shouldBeCalled` times 2
output `shouldBeCalled` (once `with` "first")
```

Matchers can also be used in mock definitions and post-hoc verification. The
following `Command` type has no `Eq` or `Show` instance, but its mock can be
defined and verified using only a predicate matcher.

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

Pass the function returned directly by `mock` to `shouldBeCalled`. Post-hoc
verification identifies a mock by function identity, so a wrapper function
cannot itself be verified.

```purescript
mockFn <- mock $ any @String :> 1977
let wrapped input = mockFn input

wrapped "Aja" `shouldEqual` 1977

mockFn `shouldBeCalled` once
-- wrapped `shouldBeCalled` once -- Cannot be verified
```

A wrapped function cannot be identified as the original mock during post-hoc
verification. When wrapping a mock, either retain the original mock function
for post-hoc verification or use `withMock` and `expects` to declare its
expectations first.

Arguments are not required when only the total number of calls matters. When
arguments are supplied with `with`, only matching calls are counted.

The following count specifications are available:

| Specification | Meaning |
| --- | --- |
| `never` | Zero times |
| `once` | One time |
| `times n` | Exactly n times |
| `atLeast n` | At least n times |
| `atMost n` | At most n times |
| `greaterThan n` | More than n times |
| `lessThan n` | Fewer than n times |

For post-hoc verification, passing arguments directly verifies that at least
one matching call was made.

```purescript
findYear `shouldBeCalled` "Aja"
findYear `shouldBeCalled` calledWith "Aja"
findYear `shouldBeCalled` anything
```

The first two lines have the same meaning. `anything` verifies that at least
one call was made, regardless of its arguments.

For a function with multiple arguments, separate the arguments with `:>` and
pass the resulting argument list as one value, just as in a mock definition:

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

Call order can be verified as follows:

```purescript
output `shouldBeCalled` inOrderWith [ "first", "second" ]
output `shouldBeCalled` inPartialOrderWith [ "first", "last" ]
```

| Specification | Meaning |
| --- | --- |
| `inOrderWith arguments` | Verifies that the complete call history matches the arguments and order |
| `inPartialOrderWith arguments` | Allows other calls between the specified calls while preserving their order |

### Verifying an argument-free Effect

When a mock function with arguments returns an `Effect`, its call is recorded
when the arguments are applied. Whether the returned `Effect` is executed does
not affect the call count.

```purescript
save <- mock $ any @String :> (pure unit :: Effect Unit)

let action = save "progress"
save `shouldBeCalled` once

liftEffect action
save `shouldBeCalled` once
```

In contrast, an argument-free `Effect a` is recorded when the `Effect` is
executed, not when it is created.

```purescript
load <- mock (pure 42 :: Effect Int)

load `shouldBeCalled` never
result <- load
load `shouldBeCalled` once
```

### Sequential responses

Multiple `onCase` entries for the same arguments return successive values.
After the final value, the final value is returned again.

```purescript
next <- mock do
  onCase $ unit :> 1
  onCase $ unit :> 2

next unit -- 1
next unit -- 2
next unit -- 2
```

Calls that match the same set of `onCase` entries share a response position.
For example, this mock returns `1` for the first call and `2` for every later
call, regardless of the argument:

```purescript
next <- mock do
  onCase $ any @String :> 1
  onCase $ any @String :> 2

next "A" -- 1
next "B" -- 2
next "A" -- 2
```

When concrete values or other matchers cause calls to match different sets of
`onCase` entries, each sequential response advances independently.

For a mock, every matching `onCase` is a candidate in the sequential response.
When matchers overlap, such as `any` and a specific value, both cases match the
specific value and their results are selected in definition order. Use the
array form `mock [ ... ]` for a Multi Mock that always returns the first match.

```purescript
firstMatch <- mock
  [ any @String :> 1
  , "A" :> 2
  ]

firstMatch "A" -- 1
firstMatch "A" -- 1
```

Stubs, sequential-response mocks, and Multi Mocks differ as follows:

| Definition | When multiple definitions match |
| --- | --- |
| `stub do onCase ...` | Always uses the first matching definition |
| `mock do onCase ...` | Uses all matching definitions as successive responses |
| `mock [ ... ]` | Always uses the first matching definition |

### Labels

Use `label` to make failures easier to identify when a test uses several stubs
or mocks.

```purescript
let findYear = stub (label "findYear") ("Aja" :> 1977)

save <- mock (label "saveProgress")
  (any @String :> (pure unit :: Effect Unit))
```

## Important constraints

- `withMock` can enclose only an `Effect` that completes synchronously. Use [post-hoc verification](#verifying-after-execution) for asynchronous code.
- `expects` can be used only inside `withMock`.
- Post-hoc verification must receive the function returned directly by `mock`. A wrapper cannot be identified as the same mock.
- The right side of `:>` is a predetermined return value. PMock has no answer-function feature that computes a result from the actual arguments; specifying a function returns that function itself.
- `mockIt` adds handling only for synchronous exceptions thrown while constructing the test-monad value.

See the corresponding examples for details.

## Failure messages

When no call matches the expected arguments, PMock shows the recorded call
whose rendered text shares the longest prefix with the expectation, along with
the call history. Differences in strings and records are identified. Count and
order failures show expected and actual values in a form appropriate to the
verification. A `label` identifies the stub or mock that failed.

For example, verifying that `findYear` was called with `"hello world"` can
produce the following message:

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

For a record, PMock also shows the differing field name and the expected and
actual values of that field.

## Catching runtime errors with purescript-spec

Functions created as stubs or mocks throw a synchronous JavaScript exception
when called with unexpected arguments. When using `purescript-spec`, use
`mockIt` in place of `it` to report that exception as a failure of the
corresponding test.

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

`mockIt` has the same type and argument order as `Test.Spec.it`. It can be used
for tests that use either stubs or mocks.

`mockIt` catches synchronous exceptions thrown while the test function is
constructing its test-monad value. It does not add another exception handler
around asynchronous `Aff` execution or computations that run after a bind;
those failures are handled by the normal `purescript-spec` runner.

## Migrating from 0.10

PMock 1.0 separates stubbing from call recording and makes both directly
callable without a PMock-specific handle. The main API replacements are:

| 0.10 | 1.0 | Notes |
| --- | --- | --- |
| `mockFun definition` | `stub definition` | Use a stub when calls do not need to be recorded |
| `mock definition` | `mock definition` | In 1.0 this returns the function directly instead of a `Mock` handle |
| `fun mock` | The function returned by `mock` | No extraction with `fun` is required |
| `namedMockFun name definition` | `stub (label name) definition` | Names are specified independently with `label` |
| `namedMock name definition` | `mock (label name) definition` | Names are specified independently with `label` |
| `mockSequence definitions` | `mock do onCase ...` | Successive `onCase` entries for the same arguments are returned in order |
| `namedMockSequence name definitions` | `mock (label name) do onCase ...` | Combine `label` and `onCase` |
| `mock [ definition1, definition2 ]` | `mock [ definition1, definition2 ]` | Preserves the legacy Multi Mock behavior of always using the first match |
| `verify mock arguments` | ``mock `shouldBeCalled` arguments`` | Verifies at least one matching call |
| ``mock `hasBeenCalledWith` arguments`` | ``mock `shouldBeCalled` arguments`` | Same as above |
| ``mock `hasNotBeenCalledWith` arguments`` | ``mock `shouldBeCalled` (never `with` arguments)`` | Verifies that no matching call was made |
| ``mock `hasBeenCalledTimes` n `with` arguments`` | ``mock `shouldBeCalled` (times n `with` arguments)`` | Verifies the matching call count |
| ``mock `hasBeenRunTimes` n`` | ``mock `shouldBeCalled` times n`` | Verifies executions of an argument-free `Effect` |
| ``mock `hasBeenCalledInOrder` arguments`` | ``mock `shouldBeCalled` inOrderWith arguments`` | Verifies the complete call history and order |
| ``mock `hasBeenCalledInPartialOrder` arguments`` | ``mock `shouldBeCalled` inPartialOrderWith arguments`` | Verifies the order of the specified calls |
| `GreaterThanEqual n` | `atLeast n` | At least n calls |
| `LessThanEqual n` | `atMost n` | At most n calls |
| `GreaterThan n` | `greaterThan n` | More than n calls |
| `LessThan n` | `lessThan n` | Fewer than n calls |
| `MatchAll matcher` | ``never `with` matcher (not <<< predicate) ...`` | Verify that no call violates the condition |
| `showCalledParams mock` | No direct replacement | Inspect the call history included in verification errors |
| `Test.PMockSpecs.mockIt` | `Test.PMock.Spec.mockIt` | The function name is unchanged; only the import changes |

Internal representations such as `Mock`, `CountVerifyMethod`, and
`VerifyMatchType` are not part of the 1.0 public API.

The PMock 0.10.2 documentation remains available:

- [PMock 0.10.2 English README](docs/README-v0.10.md)
- [PMock 0.10.2 Japanese README](docs/README-v0.10-ja.md)

## License

[MIT License](LICENSE)
