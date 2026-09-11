module Test.PMock.Internal.Message
  ( countMismatchMessage
  , countWithArgumentsMismatchMessage
  , detailedArgumentMismatchMessage
  , detailedCountMismatchMessage
  , exactOrderCountMismatchMessage
  , message
  , messageForMultiMock
  , messageFromRendered
  , messageForMultiMockFromRendered
  , mockNameLabel
  , orderMismatchMessage
  , partialOrderCountMismatchMessage
  , partialOrderMismatchMessage
  , verificationFailureMessage
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Test.PMock.Internal.Types (MockName)

countMismatchMessage :: Maybe MockName -> String -> Int -> String
countMismatchMessage name expected actual =
  countMessage name "was not called the expected number of times." expected actual

countWithArgumentsMismatchMessage :: Maybe MockName -> String -> Int -> String
countWithArgumentsMismatchMessage name expected actual =
  countMessage name
    "was not called the expected number of times with the expected arguments."
    expected
    actual

countMessage :: Maybe MockName -> String -> String -> Int -> String
countMessage name summary expected actual =
  joinWith "\n"
    [ "function" <> mockNameLabel name <> summary
    , "  expected: " <> expected
    , "   but got: " <> show actual
    ]

detailedArgumentMismatchMessage
  :: Maybe MockName
  -> String
  -> Array String
  -> String
detailedArgumentMismatchMessage name expected actuals =
  case chooseClosest expected actuals of
    Nothing -> joinWith "\n"
      [ argumentMismatchHeader name
      , "  expected: " <> expected
      , "  but the function was never called"
      ]
    Just closest ->
      let
        history = renderCallHistory closest.index actuals
        differences = structuralDiff "" expected closest.value
        differenceSection = if Array.null differences then []
          else [ formatDifferences differences ]
        closestSection =
          [ argumentMismatchHeader name
          , ""
          , "  Closest match:"
          , "    expected: " <> expected
          , "     but got: " <> closest.value
          ] <> if closest.value == "<actual value>" then []
            else [ "            " <> diffPointer expected closest.value ]
      in joinWith "\n" $
        closestSection <> differenceSection <> [ "" ] <> history

type Closest =
  { index :: Int
  , value :: String
  , score :: Int
  }

chooseClosest :: String -> Array String -> Maybe Closest
chooseClosest pivot candidates = do
  { head, tail } <- Array.uncons $ Array.mapWithIndex toCandidate candidates
  pure $ foldl chooseCloser head tail
  where
  toCandidate index value =
    { index
    , value
    , score: commonPrefixLength pivot value
    }

  chooseCloser best candidate
    | candidate.score >= best.score = candidate
    | otherwise = best

argumentMismatchHeader :: Maybe MockName -> String
argumentMismatchHeader name =
  "function" <> mockNameLabel name <>
    "was not called with the expected arguments."

commonPrefixLength :: String -> String -> Int
commonPrefixLength left right =
  Array.length $ Array.takeWhile identity $
    Array.zipWith (==) (CodeUnits.toCharArray left) (CodeUnits.toCharArray right)

diffPointer :: String -> String -> String
diffPointer expected actual =
  let
    prefixLength = commonPrefixLength expected actual
    differenceLength = max
      (CodeUnits.length expected)
      (CodeUnits.length actual) - prefixLength
  in repeatCharacter prefixLength ' ' <>
    repeatCharacter differenceLength '^'

repeatCharacter :: Int -> Char -> String
repeatCharacter count character =
  CodeUnits.fromCharArray $ Array.replicate count character

renderCallHistory :: Int -> Array String -> Array String
renderCallHistory closestIndex actuals =
  [ "  Call history (" <> renderCallCount (Array.length actuals) <> "):" ] <>
    Array.mapWithIndex renderCall actuals
  where
  renderCall index actual =
    let
      prefix
        | index == closestIndex = "    [Closest] "
        | otherwise = "              "
    in prefix <> show (index + 1) <> ". " <> actual

renderCallCount :: Int -> String
renderCallCount 1 = "1 call"
renderCallCount count = show count <> " calls"

type OrderDifference =
  { position :: Int
  , expected :: String
  , actual :: String
  }

orderMismatchMessage :: Maybe MockName -> Array OrderDifference -> String
orderMismatchMessage name differences =
  joinWith "\n" $
    [ "function" <> mockNameLabel name <>
        "was not called with the expected arguments in the expected order."
    ] <>
      (renderOrderDifference =<< differences)

renderOrderDifference :: OrderDifference -> Array String
renderOrderDifference { position, expected, actual } =
  let
    ordinal = showOrdinal position
    actualPrefix = "   but got " <> ordinal <> " call: "
  in
    [ "  expected " <> ordinal <> " call: " <> expected
    , actualPrefix <> actual
    , repeatCharacter (CodeUnits.length actualPrefix) ' ' <>
        diffPointer expected actual
    ]

partialOrderMismatchMessage
  :: Maybe MockName
  -> Array String
  -> Array String
  -> String
partialOrderMismatchMessage name expected actual =
  joinWith "\n"
    [ "function" <> mockNameLabel name <>
        "was not called with the expected arguments in the expected order."
    , "  expected order:"
    , joinWith "\n" $ ("    " <> _) <$> expected
    , "  but got:"
    , joinWith "\n" $ ("    " <> _) <$> actual
    ]

exactOrderCountMismatchMessage :: Maybe MockName -> Int -> Int -> String
exactOrderCountMismatchMessage name actual expected =
  orderCountMismatchMessage name actual expected

partialOrderCountMismatchMessage :: Maybe MockName -> Int -> Int -> String
partialOrderCountMismatchMessage name actual expected =
  orderCountMismatchMessage name actual expected

orderCountMismatchMessage :: Maybe MockName -> Int -> Int -> String
orderCountMismatchMessage name actual expected =
  joinWith "\n"
    [ "function" <> mockNameLabel name <>
        "was not called with the expected arguments in the expected order (count mismatch)."
    , "  expected: " <> show expected
    , "   but got: " <> show actual
    ]

showOrdinal :: Int -> String
showOrdinal 1 = "1st"
showOrdinal 2 = "2nd"
showOrdinal 3 = "3rd"
showOrdinal n = show n <> "th"

message :: forall a. Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  messageFromRendered name (show expected) (show actual)

messageFromRendered :: Maybe MockName -> String -> String -> String
messageFromRendered name expected actual =
  case structuralDiff "" expected actual of
    [] -> joinWith "\n" $
      [ argumentMismatchHeader name
      , "  expected: " <> expected
      , "   but got: " <> actual
      ] <> if actual == "<actual value>" then []
        else [ "            " <> diffPointer expected actual ]
    differences -> joinWith "\n"
      [ argumentMismatchHeader name
      , formatDifferences differences
      , ""
      , "Full context:"
      , "  expected: " <> expected
      , "   but got: " <> actual
      , "            " <> diffPointer expected actual
      ]

type Difference =
  { path :: String
  , expected :: String
  , actual :: String
  }

formatDifferences :: Array Difference -> String
formatDifferences [ difference ] =
  joinWith "\n"
    [ "  Specific difference in `" <> difference.path <> "`:"
    , "    expected: " <> difference.expected
    , "     but got: " <> difference.actual
    , "              " <> diffPointer difference.expected difference.actual
    ]
formatDifferences differences =
  "  Specific differences:\n" <>
    joinWith "\n" (formatDifference <$> differences)
  where
  formatDifference difference = joinWith "\n"
    [ "    - `" <> difference.path <> "`:"
    , "        expected: " <> difference.expected
    , "         but got: " <> difference.actual
    ]

structuralDiff :: String -> String -> String -> Array Difference
structuralDiff path expected actual =
  case parseRecord expected, parseRecord actual of
    Just expectedFields, Just actualFields -> expectedFields >>= \expectedField ->
      case Array.find (\actualField -> actualField.name == expectedField.name) actualFields of
        Nothing ->
          [ { path: appendField path expectedField.name
            , expected: expectedField.value
            , actual: "<missing>"
            }
          ]
        Just actualField
          | expectedField.value == actualField.value -> []
          | otherwise ->
              let nestedPath = appendField path expectedField.name
                  nested = structuralDiff nestedPath expectedField.value actualField.value
              in if Array.null nested then
                [ { path: nestedPath
                  , expected: expectedField.value
                  , actual: actualField.value
                  }
                ]
              else nested
    _, _ -> case parseList expected, parseList actual of
      Just expectedItems, Just actualItems ->
        Array.mapWithIndex
          (\index expectedItem ->
            case actualItems Array.!! index of
              Nothing ->
                [ { path: appendIndex path index
                  , expected: expectedItem
                  , actual: "<missing>"
                  }
                ]
              Just actualItem
                | expectedItem == actualItem -> []
                | otherwise ->
                    let nestedPath = appendIndex path index
                        nested = structuralDiff nestedPath expectedItem actualItem
                    in if Array.null nested then
                      [ { path: nestedPath
                        , expected: expectedItem
                        , actual: actualItem
                        }
                      ]
                    else nested
          ) expectedItems # Array.concat
      _, _ -> []

appendField :: String -> String -> String
appendField "" field = field
appendField path field = path <> "." <> field

appendIndex :: String -> Int -> String
appendIndex path index = path <> "[" <> show index <> "]"

type RenderedField = { name :: String, value :: String }

parseRecord :: String -> Maybe (Array RenderedField)
parseRecord rendered = do
  inner <- stripContainer '{' '}' rendered
  traverse parseField $ splitTopLevel ',' inner

parseField :: String -> Maybe RenderedField
parseField rendered = do
  { head, tail } <- Array.uncons $ splitTopLevel ':' rendered
  if Array.null tail then Nothing
  else Just { name: trim head, value: trim $ joinWith ":" tail }

parseList :: String -> Maybe (Array String)
parseList rendered = splitTopLevel ',' <$> stripContainer '[' ']' rendered

stripContainer :: Char -> Char -> String -> Maybe String
stripContainer open close rendered = do
  let characters = CodeUnits.toCharArray $ trim rendered
  { head, tail } <- Array.uncons $ Array.dropWhile (_ /= open) characters
  { init, last } <- Array.unsnoc tail
  if head == open && last == close then
    Just $ trim $ CodeUnits.fromCharArray init
  else Nothing

splitTopLevel :: Char -> String -> Array String
splitTopLevel separator rendered =
  go false false 0 0 0 "" [] $ CodeUnits.toCharArray rendered
  where
  go inString escaped parentheses brackets braces current parts remaining =
    case Array.uncons remaining of
      Nothing -> Array.snoc parts $ trim current
      Just { head: character, tail } ->
        if character == separator && not inString &&
          parentheses == 0 && brackets == 0 && braces == 0 then
          go inString false parentheses brackets braces ""
            (Array.snoc parts $ trim current) tail
        else
          let
            nextInString = if character == '"' && not escaped then not inString else inString
            nextEscaped = inString && character == '\\' && not escaped
            nextParentheses
              | inString = parentheses
              | character == '(' = parentheses + 1
              | character == ')' = max 0 (parentheses - 1)
              | otherwise = parentheses
            nextBrackets
              | inString = brackets
              | character == '[' = brackets + 1
              | character == ']' = max 0 (brackets - 1)
              | otherwise = brackets
            nextBraces
              | inString = braces
              | character == '{' = braces + 1
              | character == '}' = max 0 (braces - 1)
              | otherwise = braces
          in go nextInString nextEscaped nextParentheses nextBrackets nextBraces
            (current <> CodeUnits.singleton character) parts tail

detailedCountMismatchMessage
  :: Maybe MockName
  -> String
  -> String
  -> Int
  -> Array { actual :: String, matches :: Boolean }
  -> String
detailedCountMismatchMessage name expectedArguments expectedCount actualCount calls =
  joinWith "\n" $
    [ "function" <> mockNameLabel name <>
        "was not called the expected number of times with the expected arguments."
    , "  expected arguments: " <> expectedArguments
    , "  expected count:     " <> expectedCount
    , "  but got count:      " <> show actualCount
    , ""
    , "  Call history (" <> renderCallCount (Array.length calls) <> "):"
    ] <> if Array.null calls then
      [ "    (never called)" ]
    else Array.mapWithIndex renderCall calls
  where
  renderCall index call =
    let prefix = if call.matches then "    [Matched] " else "              "
    in prefix <> show (index + 1) <> ". " <> call.actual

messageForMultiMock :: forall a. Show a => Maybe MockName -> Array a -> a -> String
messageForMultiMock name expecteds actual =
  messageForMultiMockFromRendered name (show <$> expecteds) (show actual)

messageForMultiMockFromRendered :: Maybe MockName -> Array String -> String -> String
messageForMultiMockFromRendered name expecteds actual =
  joinWith "\n" $
    [ "function" <> mockNameLabel name <> "was not called with the expected arguments."
    , "  expected one of the following:"
    , joinWith "\n" $ ("    " <> _) <$> expecteds
    , "  but got:"
    , "    " <> actual
    ] <> case chooseClosest actual expecteds of
      Just closest -> [ "    " <> diffPointer closest.value actual ]
      Nothing -> []

mockNameLabel :: Maybe MockName -> String
mockNameLabel = fromMaybe " " <<< enclose " " <<< enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = map (\v -> e <> v <> e)

verificationFailureMessage :: String
verificationFailureMessage = joinWith "\n"
  [ "Error: PMock verification failed."
  , ""
  , "The function passed to 'shouldBeCalled' could not be recognized as a registered mock."
  , ""
  , "Possible causes:"
  , "  1. You passed a wrapper function around the mock."
  , "  2. You passed a normal (non-mock) function."
  , ""
  , "Solution:"
  , "  - Pass the function returned directly by 'mock'."
  , "  - If a wrapper is necessary, declare expectations with 'expects' inside 'withMock'."
  ]
