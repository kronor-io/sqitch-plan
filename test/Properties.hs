-- | Generated round trips. Names come from sqitch's name grammar, with
-- brackets, punctuation in the middle and non-ASCII word characters, and
-- appear wherever a plan allows one.
module Properties (properties) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Sqitch.Plan
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Prelude


properties :: TestTree
properties =
    testGroup
        "generated plans"
        [ testPropertyNamed "built entries render to text that parses back to them" "prop_buildParse" prop_buildParse
        , testPropertyNamed "render . parse == id on that text" "prop_renderParse" prop_renderParse
        , testPropertyNamed "a built note reads back as its trimmed text" "prop_note" prop_note
        ]


prop_buildParse :: Property
prop_buildParse = withTests 500 $ property do
    items <- forAll genItems
    let src = render (appendItems items Plan{entries = [], finalNewline = False})
    annotate (Text.unpack src)
    parsed <- evalEither (parsePlan src)
    map (shape . (.value)) parsed.entries === map shape items


prop_renderParse :: Property
prop_renderParse = withTests 500 $ property do
    items <- forAll genItems
    let src = render (appendItems items Plan{entries = [], finalNewline = False})
    parsed <- evalEither (parsePlan src)
    render parsed === src


prop_note :: Property
prop_note = property do
    t <- forAll genNoteText
    noteText (defaultNote t) === Text.strip t


-- | What an entry means, without its layout.
data Shape
    = SPragma Text (Maybe Text)
    | SChange (Maybe Operator) Text [Dependency] UTCTime Planner (Maybe Text)
    | STag Text UTCTime Planner (Maybe Text)
    | SOther
    deriving stock (Eq, Show)


shape :: Item -> Shape
shape = \case
    PragmaItem p -> SPragma p.name p.value
    ChangeItem c -> SChange c.operator c.name c.dependencies c.timestamp c.planner (noteText <$> c.note)
    TagItem t -> STag t.name t.timestamp t.planner (noteText <$> t.note)
    _ -> SOther


-- | A header, then changes, tags and reworks of earlier changes at an
-- earlier tag.
genItems :: Gen [Item]
genItems = do
    project <- genName
    n <- Gen.int (Range.linear 1 20)
    body <- go n [] []
    pure ([pragma "syntax-version" "1.0.0", pragma "project" project, Blank ""] <> body)
  where
    pragma name value =
        PragmaItem
            Pragma
                { name
                , value = Just value
                , note = Nothing
                , layout = PragmaLayout{leading = "", afterPercent = "", beforeEquals = "", afterEquals = "", trailing = ""}
                }
    go :: Int -> [Text] -> [Text] -> Gen [Item]
    go 0 _ _ = pure []
    go k changeNames tagNames = do
        -- A rework names its change and a tag in its dependency list,
        -- which cannot hold a ]: sqitch cannot rework a change, or at a
        -- tag, whose name has one.
        let noBracket = filter (not . Text.elem ']')
            rework = case (noBracket changeNames, noBracket tagNames) of
                (cs@(_ : _), ts@(_ : _)) -> [(2, genRework cs ts)]
                _ -> []
            tag = [(1, genTag) | not (null changeNames)]
        item <- Gen.frequency ([(4, genChange)] <> tag <> rework)
        let (changeNames', tagNames') = case item of
                ChangeItem c -> (c.name : changeNames, tagNames)
                TagItem t -> (changeNames, t.name : tagNames)
                _ -> (changeNames, tagNames)
        (item :) <$> go (k - 1) changeNames' tagNames'


genChange :: Gen Item
genChange = do
    c <- newChange <$> genName <*> Gen.list (Range.linear 0 5) genDependency <*> genTime <*> genPlanner <*> Gen.maybe genNoteText
    operator <- Gen.frequency [(9, pure Nothing), (1, Just <$> Gen.element [Deploy, Revert])]
    pure (ChangeItem c{operator})


genRework :: [Text] -> [Text] -> Gen Item
genRework changeNames tagNames = do
    name <- Gen.element changeNames
    tag <- Gen.element tagNames
    others <- Gen.list (Range.linear 0 3) genDependency
    let self = Dependency{kind = Requires, project = Nothing, target = OnChange name (Just tag)}
    ChangeItem <$> (newChange name (self : others) <$> genTime <*> genPlanner <*> Gen.maybe genNoteText)


genTag :: Gen Item
genTag = TagItem <$> (newTag <$> Gen.filter validTagName genName <*> genTime <*> genPlanner <*> Gen.maybe genNoteText)


genDependency :: Gen Dependency
genDependency = do
    kind <- Gen.element [Requires, Conflicts]
    project <- Gen.maybe genReference
    target <-
        Gen.choice
            [ OnChange <$> genReference <*> pure Nothing
            , OnChange <$> genReference <*> (Just <$> genReference)
            , OnTag <$> genReference
            , OnId <$> Gen.text (Range.singleton 40) (oneOf "0123456789abcdef")
            ]
    pure Dependency{kind, project, target}


-- | A name a dependency may use: any valid name, the reserved ones
-- included, except that sqitch cannot read a @]@ inside a dependency
-- list, and a bare 40-digit hex string reads as a change ID.
genReference :: Gen Text
genReference =
    Gen.filter (\n -> not (Text.elem ']' n) && not (isChangeId n)) $
        Gen.frequency [(9, genName), (1, Gen.element ["HEAD", "ROOT"])]


-- | A declarable change name from sqitch's grammar: no blanks or
-- @:\@#\\@, no ASCII punctuation first or last, a word character last.
genName :: Gen Text
genName = Gen.filter validChangeName do
    final <- Gen.element wordChars
    Gen.choice
        [ pure (Text.singleton final)
        , do
            first <- Gen.element (wordChars <> "♥€")
            middle <- Gen.list (Range.linear 0 10) (Gen.element middleChars)
            pure (Text.pack (first : middle <> [final]))
        ]
  where
    wordChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_éßøΩж日"
    middleChars = wordChars <> "-!\"$%&'()*+,./;<=>?[]^`{|}~♥€\x301"


genPlanner :: Gen Planner
genPlanner = do
    words' <- Gen.list (Range.linear 1 3) (Gen.text (Range.linear 1 8) (oneOf "abcdefXYZéñØ.'-"))
    user <- Gen.text (Range.linear 1 8) Gen.alphaNum
    pure Planner{name = Text.unwords words', email = user <> "@example.com"}


genNoteText :: Gen Text
genNoteText = Gen.text (Range.linear 0 30) (oneOf "abc XYZ#\\\n\r\t[]@:%é♥")


genTime :: Gen UTCTime
genTime = do
    day <- fromGregorian <$> Gen.integral (Range.linear 1970 2100) <*> Gen.int (Range.linear 1 12) <*> Gen.int (Range.linear 1 28)
    secs <- Gen.integral (Range.linear 0 86399)
    pure (UTCTime day (secondsToDiffTime secs))


oneOf :: String -> Gen Char
oneOf = Gen.element
