# sqitch-plan

Parse, query and render [sqitch](https://sqitch.org)'s `sqitch.plan` files.

```haskell
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Sqitch.Plan qualified as Plan

main :: IO ()
main = do
    src <- Text.IO.readFile "sqitch.plan"
    plan <- either (fail . Text.unpack . Plan.renderParseError) pure (Plan.parsePlan src)
    mapM_ (\c -> print (c.line, c.value.name, Plan.reworkTag c.value)) (Plan.changes plan)
```

- **The whole format**, following sqitch's own reader (`App::Sqitch::Plan`):
  `%pragma=value` lines, blank and `#` comment lines, change lines with
  `[dependencies]` (requires, `!conflicts`, `change@tag`, `@tag`,
  `project:change`, change IDs), planner timestamp, name and `<email>`, `# notes`, tag
  lines, and the legacy `+`/`-` operators. Sqitch's name rules are enforced, including the reserved `HEAD` / `ROOT`
  and the stricter rules for declaring tags. Notes built with `newChange` /
  `newTag` are escaped as sqitch escapes them; `noteText` undoes it.
- **Line numbers**: every entry is a `Located` value carrying its 1-based line.
- **Round trip**: whitespace is kept in each entry's `layout`, so
  `render <$> parsePlan src == Right src` for every plan the parser accepts.
  `newChange` / `newTag` build entries laid out the way `sqitch add` and
  `sqitch tag` write them, and `appendItems` adds them to a plan.
- **Queries**: `changes`, `tags`, `steps` (both, in order), `occurrences`,
  `lastOccurrence`, `changesAfter`, `isTagged`, `isReworked`, `reworkTag`,
  `reworks`, `pragmaValue` / `projectName` / `uri`.
- **Light**: depends on `base`, `text`, `containers` and `time` only.

[sqint](https://github.com/kronor-io/sqint) reads plans with sqitch-plan. It is one of the tools Kronor uses to keep a PostgreSQL schema in sqitch, with [sqiff](https://github.com/kronor-io/sqiff), [create-migration](https://github.com/kronor-io/sqitch-with-templates) and [psql-retry](https://github.com/kronor-io/psql-retry). [sqitchcraft](https://kronor-io.github.io/sqitchcraft/) shows how they fit together: the repository layout they assume, one change from an edited file to a deploy, and instructions for coding agents.

## Install

sqitch-plan is not on Hackage yet. Pin a commit from a `cabal.project`:

```cabal
source-repository-package
  type:     git
  location: https://github.com/kronor-io/sqitch-plan
  tag:      <commit>
```

## Tests

`cabal test` round-trips a synthetic plan (`test/fixtures/shop.plan`) that
uses every construct of the format, with odd whitespace, and checks its
changes, tags, pragmas, dependencies, notes and reworks against expected
values written by hand. Small hand-written plans cover each grammar feature
and each error.

When run inside a git checkout, the tests also round-trip every
`*sqitch*.plan` file tracked there, so a project that vendors this package
gets its own plans checked. The package has been tested against large
real-world plans this way.
