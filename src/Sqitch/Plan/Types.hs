-- | The shape of a @sqitch.plan@ file.
--
-- A plan is a sequence of lines. Each one is a pragma (@%name=value@), a
-- change, a tag (@\@name …@), a comment (@# …@) or blank. Every value
-- here keeps the whitespace it was written with in a @layout@ field, so
-- that "Sqitch.Plan.Render" can write a parsed plan back byte for byte.
-- A value built by hand can use the @default…Layout@ values, which
-- render the way @sqitch add@ and @sqitch tag@ write.
module Sqitch.Plan.Types (
    -- * Plans
    Plan (..),
    Located (..),
    Item (..),

    -- * Pragmas
    Pragma (..),
    PragmaLayout (..),

    -- * Changes and tags
    Change (..),
    Tag (..),
    Operator (..),
    Planner (..),
    Note (..),
    defaultNote,
    noteText,
    escapeNote,
    unescapeNote,
    EntryLayout (..),
    defaultEntryLayout,

    -- * Dependencies
    Dependency (..),
    DependencyTarget (..),
    DependencyKind (..),
    DependencyLayout (..),

    -- * Comments
    Comment (..),

    -- * Timestamps
    formatTimestamp,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Prelude


-- | A parsed plan: every line, in order.
data Plan = Plan
    { entries :: [Located Item]
    -- ^ One per line of the source, blank lines and comments included.
    , finalNewline :: Bool
    -- ^ Whether the source ended with a newline.
    }
    deriving stock (Eq, Show)


-- | A value and the 1-based line it was read from.
data Located a = Located
    { line :: Int
    , value :: a
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)


-- | What a single line holds.
data Item
    = -- | A line of nothing but whitespace; the whitespace is kept.
      Blank Text
    | CommentItem Comment
    | PragmaItem Pragma
    | ChangeItem Change
    | TagItem Tag
    deriving stock (Eq, Show)


-- | A line whose first non-blank character is @#@.
data Comment = Comment
    { indent :: Text
    -- ^ Whitespace before the @#@.
    , text :: Text
    -- ^ Everything after the @#@, as written.
    }
    deriving stock (Eq, Show)


-- | A @%name=value@ line. Sqitch reads @syntax-version@, @project@ and
-- @uri@; it keeps any other.
data Pragma = Pragma
    { name :: Text
    , value :: Maybe Text
    -- ^ 'Nothing' when the line has no @=@ at all.
    , note :: Maybe Note
    , layout :: PragmaLayout
    }
    deriving stock (Eq, Show)


data PragmaLayout = PragmaLayout
    { leading :: Text
    -- ^ Before the @%@.
    , afterPercent :: Text
    , beforeEquals :: Text
    , afterEquals :: Text
    , trailing :: Text
    -- ^ After the value, when there is no note.
    }
    deriving stock (Eq, Show)


-- | A change line:
--
-- > [+|-]name [dependencies] 2026-01-01T00:00:00Z Planner Name <planner@example.com> # note
data Change = Change
    { operator :: Maybe Operator
    -- ^ Sqitch's legacy @+@ / @-@ prefix. Absent in every modern plan.
    , name :: Text
    , dependencies :: [Dependency]
    , timestamp :: UTCTime
    -- ^ When the change was planned, at whole-second precision.
    , planner :: Planner
    , note :: Maybe Note
    , layout :: EntryLayout
    }
    deriving stock (Eq, Show)


-- | A tag line, which marks every change above it:
--
-- > @name 2026-01-01T00:00:00Z Planner Name <planner@example.com> # note
data Tag = Tag
    { name :: Text
    -- ^ Without the leading @\@@.
    , timestamp :: UTCTime
    , planner :: Planner
    , note :: Maybe Note
    , layout :: EntryLayout
    }
    deriving stock (Eq, Show)


data Operator = Deploy | Revert
    deriving stock (Eq, Show)


data Planner = Planner
    { name :: Text
    , email :: Text
    }
    deriving stock (Eq, Show)


-- | The @# …@ that ends a pragma, change or tag line.
data Note = Note
    { gap :: Text
    -- ^ Whitespace before the @#@.
    , afterHash :: Text
    -- ^ Whitespace after the @#@.
    , text :: Text
    -- ^ The note itself, up to the end of the line, as written: newlines,
    -- carriage returns and backslashes escaped as @\\n@, @\\r@ and @\\\\@.
    -- 'noteText' gives the text they stand for.
    }
    deriving stock (Eq, Show)


-- | A note as @sqitch add -n@ writes it: @ # text@, trimmed, with newlines,
-- carriage returns and backslashes escaped so it stays on one line.
defaultNote :: Text -> Note
defaultNote t = Note{gap = " ", afterHash = " ", text = escapeNote (Text.strip t)}


-- | A note's text: trimmed, with its escapes undone.
noteText :: Note -> Text
noteText n = unescapeNote (Text.strip n.text)


-- | Escape a note for a plan line the way sqitch's @format_note@ does:
-- newline as @\\n@, carriage return as @\\r@, backslash as @\\\\@.
escapeNote :: Text -> Text
escapeNote = Text.concatMap \case
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\\' -> "\\\\"
    c -> Text.singleton c


-- | Undo 'escapeNote'. Like sqitch, a backslash before anything other
-- than @n@, @r@ or another backslash is kept as is.
unescapeNote :: Text -> Text
unescapeNote t = case Text.breakOn "\\" t of
    (before, rest) -> case Text.unpack (Text.take 2 rest) of
        ['\\', 'n'] -> before <> "\n" <> unescapeNote (Text.drop 2 rest)
        ['\\', 'r'] -> before <> "\r" <> unescapeNote (Text.drop 2 rest)
        ['\\', '\\'] -> before <> "\\" <> unescapeNote (Text.drop 2 rest)
        "" -> before
        _ -> before <> Text.take 1 rest <> unescapeNote (Text.drop 1 rest)


-- | The whitespace around the fields of a change or tag line.
data EntryLayout = EntryLayout
    { leading :: Text
    -- ^ Before the operator or name.
    , afterOperator :: Text
    , afterName :: Text
    , dependencyLayout :: Maybe DependencyLayout
    -- ^ 'Nothing' for a line without brackets. A change with
    -- dependencies but no layout gets the canonical one.
    , afterTimestamp :: Text
    , beforeEmail :: Text
    , trailing :: Text
    -- ^ After the @>@, when there is no note.
    }
    deriving stock (Eq, Show)


defaultEntryLayout :: EntryLayout
defaultEntryLayout =
    EntryLayout
        { leading = ""
        , afterOperator = ""
        , afterName = " "
        , dependencyLayout = Nothing
        , afterTimestamp = " "
        , beforeEmail = " "
        , trailing = ""
        }


-- | The whitespace inside and after a @[…]@ dependency list.
data DependencyLayout = DependencyLayout
    { gaps :: [Text]
    -- ^ One more than there are dependencies: before the first, between
    -- each pair, after the last. If the count does not match (the
    -- dependencies were edited) the renderer uses single spaces.
    , afterBracket :: Text
    }
    deriving stock (Eq, Show)


-- | One entry of a change's @[…]@ list:
--
-- > [!][project:](change[@tag] | @tag | change-id)
--
-- A change that names itself at a tag (@foo [foo\@v1]@) is a rework: see
-- 'Sqitch.Plan.reworkTag'.
data Dependency = Dependency
    { kind :: DependencyKind
    , project :: Maybe Text
    , target :: DependencyTarget
    }
    deriving stock (Eq, Ord, Show)


-- | What a dependency points at.
--
-- A name here may contain @[@ but not @]@: sqitch reads a dependency list
-- as @[^]]+@ between the brackets, so a plan line naming @foo[bar]baz@ as
-- a dependency cannot be read back, by sqitch or by this package, even
-- though @foo[bar]baz@ is a valid change name.
data DependencyTarget
    = -- | A change by name, optionally as of a tag: @foo@, @foo\@v1@.
      OnChange Text (Maybe Text)
    | -- | Whatever change a tag marks: @\@v1@.
      OnTag Text
    | -- | A change by its ID, 40 lowercase hex digits (a SHA-1).
      OnId Text
    deriving stock (Eq, Ord, Show)


data DependencyKind
    = -- | The change must be deployed first.
      Requires
    | -- | Written @!name@: the change must not be deployed.
      Conflicts
    deriving stock (Eq, Ord, Show)


-- | A planned time as a plan writes it: @2026-01-01T00:00:00Z@.
formatTimestamp :: UTCTime -> Text
formatTimestamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
