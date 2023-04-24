{ name = "pmock"
, license = "MIT"
, repository = "https://github.com/pujoheadsoft/purescript-pmock"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "exceptions"
  , "maybe"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
