# Changelog

All notable changes to this project will be documented in this file.

## [1.1.0] - 2026-09-12

### Added

- Added `andThen` for declaring successive responses on a single `onCase`.

### Changed

- Changed `mock do onCase ...` to use first-match-wins semantics. Each selected case now maintains its own response position; repeated overlapping `onCase` entries no longer form an implicit response sequence.

## [1.0.1] - 2026-09-11

### Documentation

- Clarified that `withMock` and `expects` provide declaration-first verification and are not specific to wrapped mock functions.
- Clarified that wrapped functions cannot be identified as their original mocks during post-hoc verification.

## [1.0.0] - 2026-09-11

### Added

- Added `stub` for creating test doubles that do not record calls.
- Added directly callable mocks that can be passed to function- and `Effect`-typed dependencies without extracting a function from a handle.
- Added `withMock` and `expects` for declaring expectations with a mock and verifying them when the scope exits.
- Added a shared expectation DSL for total call counts, argument-specific call counts, exact order, and partial order.
- Added `never`, `once`, `times`, `atLeast`, `atMost`, `greaterThan`, and `lessThan` call-count specifications.
- Added predicate matchers that do not require `Eq` or `Show` for the argument type, with optional actual-value rendering through `matcherBy`.
- Added labels and diagnostics that can show the call with the longest matching rendered prefix, string and record differences, and call history.
- Added `Test.PMock.Spec.mockIt` for reporting synchronous Stub and Mock exceptions as `purescript-spec` failures.

### Changed

- Changed `mock` to return the mocked function or `Effect` directly instead of a `Mock` handle.
- Replaced constructor-specific naming APIs with the independent `label` modifier.
- Replaced the 0.10 post-hoc verification functions with `shouldBeCalled` and the shared expectation DSL.
- Changed sequential responses to use `mock do onCase ...`; calls matching the same complete set of cases share a response position.
- Preserved the array-based Multi Mock behavior: `mock [ ... ]` continues to use the first matching definition.
- Kept argument-free `Effect a` recording at execution time and retained function construction without a fixed arity limit.

### Removed

- Removed the public `Mock` handle and the need for `fun`.
- Removed `mockFun`, `namedMockFun`, `namedMock`, `namedMockSequence`, and the old `hasBeen*` verification functions from the recommended API.
- Removed internal verification representations such as `CountVerifyMethod` and `VerifyMatchType` from the public API.

See the README migration table for the complete 0.10-to-1.0 API mapping.

## [0.10.2] - 2026-09-09

### Fixed

- Migrated to the current Spago configuration format and separated the test-only `aff` dependency from library dependencies so Registry publishing succeeds.

## [0.10.1] - 2026-09-09

### Fixed

- Added a Registry manifest that separates library dependencies from test-only dependencies, allowing the package to pass Registry validation.

## [0.10.0] - 2026-09-08

### Added

- Support for mocking argument-free `Effect a` actions and verifying their execution count with `hasBeenRunTimes`.
- Sequential responses with `mockSequence` and `namedMockSequence`, including independent response positions for each argument combination.
- Recursive mock-function construction with no fixed maximum arity.

### Changed

- Split the implementation and tests into focused modules without changing the existing public API.
- Kept existing Multi Mock behavior unchanged: duplicate argument definitions continue to select the first matching response.
