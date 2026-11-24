module Main (main) where

import Control.Exception (SomeException, try)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time (UTCTime (..), fromGregorian)
import Properties (properties)
import Sqitch.Plan
import System.Process (readProcess)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude


main :: IO ()
main = do
    found <- gitPlans
    defaultMain $
        testGroup
            "sqitch-plan"
            [ roundTrips
            , fixtureTests
            , realWorld found
            , pragmaTests
            , changeTests
            , tagTests
            , reworkTests
            , queryTests
            , errorTests
            , buildingTests
            , changeIdTests
            , declarationTests
            , noteEscapeTests
            , emptyDependencyTests
            , grammarEdgeTests
            , properties
            ]


-- | A synthetic plan with every construct the parser knows. Its changes
-- and tags, one @line name@ per entry, are written out by hand next to it.
shopPlan :: FilePath
shopPlan = "test/fixtures/shop.plan"


-- | Every plan file in the git checkout the tests run in, if any:
-- whatever real plans sit next to the package. Outside a checkout (an
-- sdist build, say) there are none and the group is empty.
gitPlans :: IO [FilePath]
gitPlans = do
    top <- try (readProcess "git" ["rev-parse", "--show-toplevel"] "")
    case top of
        Left (_ :: SomeException) -> pure []
        Right out -> do
            let root = Text.unpack (Text.strip (Text.pack out))
            files <- try (readProcess "git" ["-C", root, "ls-files", "*sqitch*.plan"] "")
            pure case files of
                Left (_ :: SomeException) -> []
                Right ls -> [root <> "/" <> l | l <- lines ls, not (null l)]


parseOk :: Text -> IO Plan
parseOk src = either (assertFailure . Text.unpack . renderParseError) pure (parsePlan src)


roundTrips :: TestTree
roundTrips =
    testGroup "render . parse == id" $
        [ testCase shopPlan do
            src <- Text.IO.readFile shopPlan
            plan <- parseOk src
            render plan @?= src
        ]
            <> [ testCase (show src) (parseOk src >>= \p -> render p @?= src)
               | src <-
                    [ ""
                    , "\n"
                    , "\n\n"
                    , "%project=x"
                    , "  \t\n# comment\n  #indented comment\n"
                    , "% name = value   \n"
                    , "%flag # note\n"
                    , "foo 2026-01-01T00:00:00Z Ada <ada@example.com>"
                    , "foo  2026-01-01T00:00:00Z   Ada Lovelace  <ada@example.com>  #  note  \n"
                    , "foo 2026-01-01T00:00:00Z Ada <ada@example.com>#tight\n"
                    , "foo 2026-01-01T00:00:00Z Ada <ada@example.com>   \n"
                    , "  foo[ bar  baz ]2026-01-01T00:00:00Z Ada <ada@example.com>\n"
                    , "+ foo 2026-01-01T00:00:00Z Ada <ada@example.com>\n"
                    , "  @v1.0  2026-01-01T00:00:00Z Ada <ada@example.com> # a # b\n"
                    , "foo 2026-01-01T00:00:00Z Ada <ada@example.com>\r\n"
                    ]
               ]


pragmaTests :: TestTree
pragmaTests =
    testGroup
        "pragmas"
        [ testCase "the three sqitch reads, and any other" do
            plan <- parseOk "%syntax-version=1.0.0\n%project=flipr\n%uri=https://github.com/sqitchers/sqitch-intro/\n%custom=yes\n"
            syntaxVersion plan @?= Just "1.0.0"
            projectName plan @?= Just "flipr"
            uri plan @?= Just "https://github.com/sqitchers/sqitch-intro/"
            pragmaValue "custom" plan @?= Just "yes"
            map (.line) (pragmas plan) @?= [1, 2, 3, 4]
        , testCase "spaces around = are not part of name or value" do
            plan <- parseOk "  %  project =  flipr  # the project\n"
            projectName plan @?= Just "flipr"
            map (fmap noteText . (.value.note)) (pragmas plan) @?= [Just "the project"]
        , testCase "a pragma without = has no value" do
            plan <- parseOk "%flag\n"
            map (.value.value) (pragmas plan) @?= [Nothing]
        , testCase "a pragma without = counts as 1, and a later one wins" do
            plan <- parseOk "%custom=old\n%custom\n%other\n"
            pragmaValue "custom" plan @?= Just "1"
            pragmaValue "other" plan @?= Just "1"
            pragmaValue "missing" plan @?= Nothing
            render plan @?= "%custom=old\n%custom\n%other\n"
        , testCase "the last of a repeated pragma wins" do
            plan <- parseOk "%project=a\n%project=b\n"
            projectName plan @?= Just "b"
        ]


at :: Integer -> Int -> Int -> UTCTime
at y m d = UTCTime (fromGregorian y m d) 0


changeTests :: TestTree
changeTests =
    testGroup
        "changes"
        [ testCase "every field" do
            plan <- parseOk "users [roles !legacy other:widgets @v1.0 flips@v1.1 !proj:x@y] 2013-12-30T00:00:00Z Marge N. O'Vera <marge@example.com> # Creates table to track our users.\n"
            [Located{line = 1, value = c}] <- pure (changes plan)
            c.operator @?= Nothing
            c.name @?= "users"
            c.timestamp @?= at 2013 12 30
            c.planner @?= Planner{name = "Marge N. O'Vera", email = "marge@example.com"}
            fmap noteText c.note @?= Just "Creates table to track our users."
            c.dependencies
                @?= [ Dependency{kind = Requires, project = Nothing, target = OnChange "roles" Nothing}
                    , Dependency{kind = Conflicts, project = Nothing, target = OnChange "legacy" Nothing}
                    , Dependency{kind = Requires, project = Just "other", target = OnChange "widgets" Nothing}
                    , Dependency{kind = Requires, project = Nothing, target = OnTag "v1.0"}
                    , Dependency{kind = Requires, project = Nothing, target = OnChange "flips" (Just "v1.1")}
                    , Dependency{kind = Conflicts, project = Just "proj", target = OnChange "x" (Just "y")}
                    ]
        , testCase "no note, no dependencies" do
            plan <- parseOk "appschema 2013-12-30T00:00:00Z Ada <ada@example.com>\n"
            map (\c -> (c.value.name, c.value.dependencies, c.value.note)) (changes plan) @?= [("appschema", [], Nothing)]
        , testCase "a # inside the note is part of it" do
            plan <- parseOk "foo 2026-01-01T00:00:00Z Ada <ada@example.com> # fixes #3217\n"
            map (fmap noteText . (.value.note)) (changes plan) @?= [Just "fixes #3217"]
        , testCase "a planner name may have spaces and non-ASCII letters" do
            plan <- parseOk "foo 2026-01-01T00:00:00Z María José Ñúñez <maria@example.com>\n"
            map (.value.planner.name) (changes plan) @?= ["María José Ñúñez"]
        , testCase "legacy + and - operators" do
            plan <- parseOk "+foo 2026-01-01T00:00:00Z Ada <ada@example.com>\n- bar 2026-01-01T00:00:00Z Ada <ada@example.com>\n"
            map (\c -> (c.value.operator, c.value.name)) (changes plan) @?= [(Just Deploy, "foo"), (Just Revert, "bar")]
        , testCase "blank and comment lines are kept but are not changes" do
            plan <- parseOk "# header\n\nfoo 2026-01-01T00:00:00Z Ada <ada@example.com>\n  # between\nbar 2026-01-01T00:00:00Z Ada <ada@example.com>\n"
            map (\c -> (c.line, c.value.name)) (changes plan) @?= [(3, "foo"), (5, "bar")]
            map (kindOf . (.value)) plan.entries
                @?= [ "comment  header"
                    , "blank "
                    , "change foo"
                    , "comment    between"
                    , "change bar"
                    ]
            map (.line) plan.entries @?= [1, 2, 3, 4, 5]
        ]
  where
    kindOf :: Item -> Text
    kindOf = \case
        Blank t -> "blank " <> t
        CommentItem c -> "comment " <> c.indent <> c.text
        PragmaItem p -> "pragma " <> p.name
        ChangeItem c -> "change " <> c.name
        TagItem t -> "tag " <> t.name


tagTests :: TestTree
tagTests =
    testGroup
        "tags"
        [ testCase "a tag line" do
            plan <- parseOk "foo 2026-01-01T00:00:00Z Ada <ada@example.com>\n@v1.0 2026-01-02T00:00:00Z Ada <ada@example.com> # Tag v1.0.\n"
            [Located{line = 2, value = t}] <- pure (tags plan)
            t.name @?= "v1.0"
            t.timestamp @?= at 2026 1 2
            fmap noteText t.note @?= Just "Tag v1.0."
        , testCase "steps interleave changes and tags in plan order" do
            plan <- parseOk "a 2026-01-01T00:00:00Z Ada <a@x>\n@t1 2026-01-01T00:00:00Z Ada <a@x>\nb 2026-01-01T00:00:00Z Ada <a@x>\n"
            map (\s -> (s.line, stepName s.value)) (steps plan) @?= [(1, "a"), (2, "@t1"), (3, "b")]
        ]
  where
    stepName (StepChange c) = c.name
    stepName (StepTag t) = "@" <> t.name


reworkPlan :: Text
reworkPlan =
    Text.unlines
        [ "%syntax-version=1.0.0"
        , "%project=r"
        , ""
        , "foo 2026-01-01T00:00:00Z Ada <a@x>"
        , "bar [foo] 2026-01-01T00:00:00Z Ada <a@x>"
        , "@v1 2026-01-02T00:00:00Z Ada <a@x>"
        , "foo [foo@v1] 2026-01-03T00:00:00Z Ada <a@x> # rework foo"
        , "baz [bar@v1] 2026-01-03T00:00:00Z Ada <a@x>"
        , "@v2 2026-01-04T00:00:00Z Ada <a@x>"
        , "foo [foo@v2 bar] 2026-01-05T00:00:00Z Ada <a@x>"
        , "qux 2026-01-05T00:00:00Z Ada <a@x>"
        ]


reworkTests :: TestTree
reworkTests =
    testGroup
        "reworks"
        [ testCase "which changes are reworked" do
            plan <- parseOk reworkPlan
            isReworked "foo" plan @?= True
            isReworked "bar" plan @?= False
            isReworked "nope" plan @?= False
        , testCase "the tag a rework names its earlier self at" do
            plan <- parseOk reworkPlan
            map (\c -> (c.line, reworkTag c.value)) (changes plan)
                @?= [(4, Nothing), (5, Nothing), (7, Just "v1"), (8, Nothing), (10, Just "v2"), (11, Nothing)]
        , testCase "each later occurrence and the one before it" do
            plan <- parseOk reworkPlan
            map (\r -> (r.change.line, r.previous.line, r.tag)) (reworks plan) @?= [(7, 4, Just "v1"), (10, 7, Just "v2")]
        , testCase "occurrences" do
            plan <- parseOk reworkPlan
            map (.line) (occurrences "foo" plan) @?= [4, 7, 10]
            fmap (.line) (lastOccurrence "foo" plan) @?= Just 10
        ]


queryTests :: TestTree
queryTests =
    testGroup
        "queries"
        [ testCase "changes after a change follow its last occurrence" do
            plan <- parseOk reworkPlan
            fmap (map (.value.name)) (changesAfter "bar" plan) @?= Just ["foo", "baz", "foo", "qux"]
            fmap (map (.value.name)) (changesAfter "foo" plan) @?= Just ["qux"]
            fmap (map (.value.name)) (changesAfter "qux" plan) @?= Just []
            fmap (map (.value.name)) (changesAfter "nope" plan) @?= Nothing
        , testCase "a change is tagged when a tag follows its last occurrence" do
            plan <- parseOk reworkPlan
            isTagged "bar" plan @?= True
            isTagged "baz" plan @?= True
            isTagged "foo" plan @?= False
            isTagged "qux" plan @?= False
            map (.value.name) (tagsAfterLine 5 plan) @?= ["v1", "v2"]
        ]


errorTests :: TestTree
errorTests =
    testGroup
        "errors"
        [ testCase "reports the line" do
            fmap (.line) (either Just (const Nothing) (parsePlan "%project=x\n\nfoo 2026-01-01T00:00:00Z Ada\n")) @?= Just 3
        , rejects "no email" "foo 2026-01-01T00:00:00Z Ada\n"
        , rejects "unclosed email" "foo 2026-01-01T00:00:00Z Ada <ada@example.com\n"
        , rejects "text after the email" "foo 2026-01-01T00:00:00Z Ada <a@x> extra\n"
        , rejects "no planner" "foo 2026-01-01T00:00:00Z <a@x>\n"
        , rejects "no timestamp" "foo Ada <a@x>\n"
        , rejects "fractional seconds" "foo 2026-01-01T00:00:00.5Z Ada <a@x>\n"
        , rejects "unpadded month" "foo 2026-1-01T00:00:00Z Ada <a@x>\n"
        , rejects "a tag with dependencies" "@v1 [foo] 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "unclosed dependency list" "foo [bar 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "an empty dependency" "foo [!] 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "an empty project" "foo [:bar] 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "a pragma without a name" "%=x\n"
        , testCase "names" do
            map (\n -> (n, validName n)) ["foo", "foo/bar", "2022-08-26-x", "a_", "_a", "v1.0", "foo-1.0", "x^y"]
                @?= map (,True) ["foo", "foo/bar", "2022-08-26-x", "a_", "_a", "v1.0", "foo-1.0", "x^y"]
            map (\n -> (n, validName n)) ["", "-foo", "foo-", "foo.", "a:b", "a@b", "a#b", "a\\b", "a b", "foo^2", "foo~12", "foo/3", "x=1", "y%2"]
                @?= map (,False) ["", "-foo", "foo-", "foo.", "a:b", "a@b", "a#b", "a\\b", "a b", "foo^2", "foo~12", "foo/3", "x=1", "y%2"]
        ]
  where
    rejects what src = testCase what (assertBool ("parsed: " <> show src) (isLeft (parsePlan src)))


buildingTests :: TestTree
buildingTests =
    testGroup
        "building"
        [ testCase "a new change renders like sqitch add" do
            let c =
                    newChange
                        "foo"
                        [Dependency{kind = Requires, project = Nothing, target = OnChange "foo" (Just "v1")}, Dependency{kind = Conflicts, project = Nothing, target = OnChange "bar" Nothing}]
                        (at 2026 1 1)
                        Planner{name = "Ada Lovelace", email = "ada@example.com"}
                        (Just "hello")
            renderItem (ChangeItem c) @?= "foo [foo@v1 !bar] 2026-01-01T00:00:00Z Ada Lovelace <ada@example.com> # hello"
            renderItem (ChangeItem (newChange "foo" [] (at 2026 1 1) Planner{name = "Ada", email = "a@x"} Nothing))
                @?= "foo 2026-01-01T00:00:00Z Ada <a@x>"
        , testCase "a new tag renders like sqitch tag" do
            renderItem (TagItem (newTag "v1" (at 2026 1 1) Planner{name = "Ada", email = "a@x"} (Just "release")))
                @?= "@v1 2026-01-01T00:00:00Z Ada <a@x> # release"
        , testCase "appending numbers on and parses back" do
            plan <- parseOk "%project=x\n\nfoo 2026-01-01T00:00:00Z Ada <a@x>\n"
            let plan' = appendItems [TagItem (newTag "v1" (at 2026 1 2) Planner{name = "Ada", email = "a@x"} Nothing)] plan
            render plan' @?= "%project=x\n\nfoo 2026-01-01T00:00:00Z Ada <a@x>\n@v1 2026-01-02T00:00:00Z Ada <a@x>\n"
            map (.line) (tags plan') @?= [4]
            reparsed <- parseOk (render plan')
            reparsed @?= plan'
        ]


realWorld :: [FilePath] -> TestTree
realWorld paths =
    testGroup
        "render . parse == id, for the plans in this git checkout"
        [ testCase path (Text.IO.readFile path >>= \src -> parseOk src >>= \p -> render p @?= src)
        | path <- paths
        ]


fixtureTests :: TestTree
fixtureTests =
    testGroup
        "shop.plan"
        [ testCase "changes, in order with their lines" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            expected <- Text.lines <$> Text.IO.readFile "test/fixtures/shop.changes"
            [Text.pack (show c.line) <> " " <> c.value.name | c <- changes plan] @?= expected
        , testCase "tags, in order with their lines" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            expected <- Text.lines <$> Text.IO.readFile "test/fixtures/shop.tags"
            [Text.pack (show t.line) <> " " <> t.value.name | t <- tags plan] @?= expected
        , testCase "pragmas" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            [(p.line, p.value.name, p.value.value) | p <- pragmas plan]
                @?= [ (1, "syntax-version", Just "1.0.0")
                    , (2, "project", Just "shop")
                    , (3, "uri", Just "https://example.com/shop/")
                    , (4, "custom-pragma", Just "some value")
                    , (5, "flag", Nothing)
                    ]
        , testCase "reworks, including one with further dependencies" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            [(r.change.line, r.previous.line, r.tag) | r <- reworks plan]
                @?= [(18, 12, Just "v1.0"), (24, 11, Just "v1.1"), (28, 18, Just "v1.1")]
            map renderDependency (concatMap (.value.dependencies) (drop 1 (occurrences "orders" plan)))
                @?= ["orders@v1.0", "pricing", "orders@v1.1", "invoices"]
        , testCase "cross-project and tag dependencies" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            map (\c -> map renderDependency c.value.dependencies) (filter (\c -> c.value.name `elem` ["audit", "pricing", "order_lines"]) (changes plan))
                @?= [ ["orders", "!legacy_lines"]
                    , ["payments:ledger", "payments:ledger@v2.0", "!payments:old_ledger"]
                    , ["@v1.0", "widgets@v1.0"]
                    ]
        , testCase "notes and planners" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            [(c.line, fmap noteText c.value.note) | c <- changes plan, c.line `elem` [12, 19, 21, 28]]
                @?= [ (12, Just "orders, see issue #12")
                    , (19, Just "odd spacing")
                    , (21, Just "no space before the hash")
                    , (28, Just "second rework, notes may hold # and ## too")
                    ]
            [c.value.planner | c <- changes plan, c.line == 14]
                @?= [Planner{name = "Jos\233 \209\250\241ez \216rsted", email = "jose@example.com"}]
        , testCase "legacy operators" do
            plan <- parseOk =<< Text.IO.readFile shopPlan
            [(c.value.name, op) | c <- changes plan, Just op <- [c.value.operator]]
                @?= [("invoices", Deploy), ("legacy_lines", Revert)]
        ]


hexId :: Text
hexId = "0123456789abcdef0123456789abcdef01234567"


changeIdTests :: TestTree
changeIdTests =
    testGroup
        "change IDs"
        [ testCase "40 lowercase hex digits are an ID, bare or project-qualified" do
            let src = "foo [" <> hexId <> " !other:" <> hexId <> " " <> Text.take 39 hexId <> " " <> Text.toUpper hexId <> "] 2026-01-01T00:00:00Z Ada <a@x>\n"
            plan <- parseOk src
            map (\c -> c.value.dependencies) (changes plan)
                @?= [
                        [ Dependency{kind = Requires, project = Nothing, target = OnId hexId}
                        , Dependency{kind = Conflicts, project = Just "other", target = OnId hexId}
                        , Dependency{kind = Requires, project = Nothing, target = OnChange (Text.take 39 hexId) Nothing}
                        , Dependency{kind = Requires, project = Nothing, target = OnChange (Text.toUpper hexId) Nothing}
                        ]
                    ]
            render plan @?= src
        , testCase "renderDependency writes an ID back" do
            renderDependency Dependency{kind = Conflicts, project = Just "p", target = OnId hexId} @?= "!p:" <> hexId
        ]


declarationTests :: TestTree
declarationTests =
    testGroup
        "declared names"
        [ rejects "a change named HEAD" "HEAD 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "a change named ROOT" "ROOT 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "a tag named HEAD" "@HEAD 2026-01-01T00:00:00Z Ada <a@x>\n"
        , rejects "a change that starts with 40 hex digits" ("" <> hexId <> "_x 2026-01-01T00:00:00Z Ada <a@x>\n")
        , rejects "a tag that is 40 hex digits" ("@" <> hexId <> " 2026-01-01T00:00:00Z Ada <a@x>\n")
        , rejects "a tag with a /" "@v1/x 2026-01-01T00:00:00Z Ada <a@x>\n"
        , testCase "a change name may have a /" do
            plan <- parseOk "app/foo 2026-01-01T00:00:00Z Ada <a@x>\n"
            map (.value.name) (changes plan) @?= ["app/foo"]
        , testCase "dependencies may name HEAD, ROOT and tags with a /" do
            plan <- parseOk "foo [bar@HEAD @ROOT HEAD baz@v1/x] 2026-01-01T00:00:00Z Ada <a@x>\n"
            map (map renderDependency . (.value.dependencies)) (changes plan) @?= [["bar@HEAD", "@ROOT", "HEAD", "baz@v1/x"]]
        , testCase "the predicates" do
            map validChangeName ["HEAD", "ROOT", hexId, hexId <> "x", "head", Text.take 39 hexId, "a/b"] @?= [False, False, False, False, True, True, True]
            map validTagName ["v1", "v1/x", "HEAD"] @?= [True, False, False]
            map isChangeId [hexId, Text.toUpper hexId, Text.take 39 hexId, hexId <> "0"] @?= [True, False, False, False]
        ]
  where
    rejects what src = testCase what (assertBool ("parsed: " <> show src) (isLeft (parsePlan src)))


noteEscapeTests :: TestTree
noteEscapeTests =
    testGroup
        "note escapes"
        [ testCase "a new change's note is escaped like sqitch's format_note" do
            let c = newChange "foo" [] (at 2026 1 1) Planner{name = "Ada", email = "a@x"} (Just "line one\nline two\r\nC:\\path")
            renderItem (ChangeItem c) @?= "foo 2026-01-01T00:00:00Z Ada <a@x> # line one\\nline two\\r\\nC:\\\\path"
            fmap noteText c.note @?= Just "line one\nline two\r\nC:\\path"
        , testCase "a new tag's note too" do
            renderItem (TagItem (newTag "v1" (at 2026 1 1) Planner{name = "Ada", email = "a@x"} (Just "a\nb")))
                @?= "@v1 2026-01-01T00:00:00Z Ada <a@x> # a\\nb"
        , testCase "a parsed note keeps its raw form and unescapes on reading" do
            let src = "foo 2026-01-01T00:00:00Z Ada <a@x> # two\\nlines and a \\\\ and \\q\n"
            plan <- parseOk src
            render plan @?= src
            map (fmap (.text) . (.value.note)) (changes plan) @?= [Just "two\\nlines and a \\\\ and \\q"]
            map (fmap noteText . (.value.note)) (changes plan) @?= [Just "two\nlines and a \\ and \\q"]
        , testCase "escape and unescape are inverses" do
            let t = "a\\b\nc\rd\\\\n"
            unescapeNote (escapeNote t) @?= t
        ]


emptyDependencyTests :: TestTree
emptyDependencyTests =
    testGroup
        "empty dependency lists"
        [ testCase "removing every dependency drops the brackets" do
            plan <- parseOk "foo [bar] 2026-01-01T00:00:00Z Ada <a@x>\ntight[bar]2026-01-01T00:00:00Z Ada <a@x>\n"
            map (renderItem . ChangeItem . (\c -> c.value{dependencies = []})) (changes plan)
                @?= ["foo 2026-01-01T00:00:00Z Ada <a@x>", "tight 2026-01-01T00:00:00Z Ada <a@x>"]
        , testCase "[] and [ ] do not parse" do
            assertBool "[]" (isLeft (parsePlan "foo [] 2026-01-01T00:00:00Z Ada <a@x>\n"))
            assertBool "[ ]" (isLeft (parsePlan "foo [ ] 2026-01-01T00:00:00Z Ada <a@x>\n"))
        ]


grammarEdgeTests :: TestTree
grammarEdgeTests =
    testGroup
        "names and timestamps by sqitch's grammar"
        [ testCase "a change name may contain brackets" do
            let src = "foo[bar]baz 2026-01-01T00:00:00Z Ada <a@x>\nqux[a]b[dep] 2026-01-01T00:00:00Z Ada <a@x>\nplain[dep] 2026-01-01T00:00:00Z Ada <a@x>\n"
            plan <- parseOk src
            map (\c -> (c.value.name, map renderDependency c.value.dependencies)) (changes plan)
                @?= [("foo[bar]baz", []), ("qux[a]b", ["dep"]), ("plain", ["dep"])]
            render plan @?= src
        , testCase "a tag name may contain brackets" do
            let src = "foo 2026-01-01T00:00:00Z Ada <a@x>\n@v1[rc]x 2026-01-01T00:00:00Z Ada <a@x>\n"
            plan <- parseOk src
            map (.value.name) (tags plan) @?= ["v1[rc]x"]
            render plan @?= src
            assertBool "tag with dependencies" (isLeft (parsePlan "@v1[rc] 2026-01-01T00:00:00Z Ada <a@x>\n"))
        , testCase "no blank is needed after the timestamp" do
            let src = "foo 2026-01-01T00:00:00ZAda Lovelace <a@x>\n@v1 2026-01-02T00:00:00ZAda <a@x>\n"
            plan <- parseOk src
            map (\c -> (c.value.timestamp, c.value.planner.name)) (changes plan) @?= [(at 2026 1 1, "Ada Lovelace")]
            map (\t -> (t.value.timestamp, t.value.planner.name)) (tags plan) @?= [(at 2026 1 2, "Ada")]
            render plan @?= src
        , testCase "the planner's name and email need a blank between them" do
            assertBool "Ada<a@x>" (isLeft (parsePlan "foo 2026-01-01T00:00:00Z Ada<a@x>\n"))
        , testCase "a name must end in a word character" do
            map validChangeName ["foo\9829", "foo_", "caf\233", "x\8255"] @?= [False, True, True, True]
            validTagName "v1\9829" @?= False
            assertBool "change" (isLeft (parsePlan "foo\9829 2026-01-01T00:00:00Z Ada <a@x>\n"))
            assertBool "tag" (isLeft (parsePlan "@v1\9829 2026-01-01T00:00:00Z Ada <a@x>\n"))
        ]
