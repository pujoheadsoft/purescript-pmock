{ name = "pmock"
, license = "MIT"
, repository = "https://github.com/pujoheadsoft/purescript-pmock"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
