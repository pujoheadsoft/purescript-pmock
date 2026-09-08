# Changelog

All notable changes to this project will be documented in this file.

## [0.10.0] - 2026-09-08

### Added

- Support for mocking argument-free `Effect a` actions and verifying their execution count with `hasBeenRunTimes`.
- Sequential responses with `mockSequence` and `namedMockSequence`, including independent response positions for each argument combination.
- Recursive mock-function construction with no fixed maximum arity.

### Changed

- Split the implementation and tests into focused modules without changing the existing public API.
- Kept existing Multi Mock behavior unchanged: duplicate argument definitions continue to select the first matching response.
