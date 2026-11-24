-- | Sqitch's @sqitch.plan@ format: parse it, query it, write it back.
--
-- > import Sqitch.Plan qualified as Plan
-- >
-- > main = do
-- >     src <- Data.Text.IO.readFile "sqitch.plan"
-- >     plan <- either (fail . Text.unpack . Plan.renderParseError) pure (Plan.parsePlan src)
-- >     mapM_ (print . (.value.name)) (Plan.changes plan)
--
-- See @sqitch-plan(5)@ and @sqitchchanges(3)@ for the format itself.
module Sqitch.Plan (
    module Sqitch.Plan.Types,

    -- * Reading and writing
    parsePlan,
    ParseError (..),
    renderParseError,
    render,
    renderItem,
    renderDependency,
    validName,
    validChangeName,
    validTagName,
    isChangeId,

    -- * Pragmas
    pragmas,
    pragmaValue,
    syntaxVersion,
    projectName,
    uri,

    -- * Changes and tags
    Step (..),
    steps,
    changes,
    tags,
    occurrences,
    lastOccurrence,
    changesAfter,
    changesAfterLine,
    tagsAfterLine,
    isTagged,

    -- * Reworks
    Rework (..),
    isReworked,
    reworkTag,
    reworks,

    -- * Building
    newChange,
    newTag,
    appendItems,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Sqitch.Plan.Parse
import Sqitch.Plan.Render
import Sqitch.Plan.Types
import Prelude


----------------------------------------------------------------------------------------------------
-- Pragmas

pragmas :: Plan -> [Located Pragma]
pragmas = located (\case PragmaItem p -> Just p; _ -> Nothing)


-- | The value of a pragma. If a plan repeats one the last wins, and a
-- pragma written without @=value@ counts as @1@, both as in sqitch. The
-- entry itself keeps what was written ('Pragma.value').
pragmaValue :: Text -> Plan -> Maybe Text
pragmaValue name plan =
    Map.lookup name (Map.fromList [(p.value.name, fromMaybe "1" p.value.value) | p <- pragmas plan])


syntaxVersion, projectName, uri :: Plan -> Maybe Text
syntaxVersion = pragmaValue "syntax-version"
projectName = pragmaValue "project"
uri = pragmaValue "uri"


----------------------------------------------------------------------------------------------------
-- Changes and tags

-- | A change or a tag: the lines that are deploy steps.
data Step = StepChange Change | StepTag Tag
    deriving stock (Eq, Show)


-- | Changes and tags in plan order.
steps :: Plan -> [Located Step]
steps = located \case
    ChangeItem c -> Just (StepChange c)
    TagItem t -> Just (StepTag t)
    _ -> Nothing


-- | Every change in plan order, which is deploy order. A reworked
-- change appears once per occurrence.
changes :: Plan -> [Located Change]
changes = located (\case ChangeItem c -> Just c; _ -> Nothing)


tags :: Plan -> [Located Tag]
tags = located (\case TagItem t -> Just t; _ -> Nothing)


-- | Every occurrence of a change name, in plan order: more than one
-- when the change has been reworked.
occurrences :: Text -> Plan -> [Located Change]
occurrences name = filter (\c -> c.value.name == name) . changes


-- | The latest occurrence of a change: the one a deploy ends on.
lastOccurrence :: Text -> Plan -> Maybe (Located Change)
lastOccurrence name = listToMaybe . reverse . occurrences name


-- | The changes after the last occurrence of the named one, or 'Nothing'
-- if the plan has no such change.
changesAfter :: Text -> Plan -> Maybe [Located Change]
changesAfter name plan = (\c -> changesAfterLine c.line plan) <$> lastOccurrence name plan


-- | The changes below a line.
changesAfterLine :: Int -> Plan -> [Located Change]
changesAfterLine n = filter (\c -> c.line > n) . changes


-- | The tags below a line.
tagsAfterLine :: Int -> Plan -> [Located Tag]
tagsAfterLine n = filter (\t -> t.line > n) . tags


-- | Whether a tag follows the change's last occurrence. Sqitch can only
-- rework a change that is tagged.
isTagged :: Text -> Plan -> Bool
isTagged name plan = maybe False (\c -> not (null (tagsAfterLine c.line plan))) (lastOccurrence name plan)


----------------------------------------------------------------------------------------------------
-- Reworks

-- | A change that occurs again after an earlier occurrence of itself.
data Rework = Rework
    { change :: Located Change
    -- ^ The later occurrence.
    , previous :: Located Change
    -- ^ The occurrence directly before it.
    , tag :: Maybe Text
    -- ^ 'reworkTag' of the later occurrence: the tag its scripts for
    -- the previous version are named after (@deploy/name\@tag.sql@).
    }
    deriving stock (Eq, Show)


-- | Whether the plan has the change more than once.
isReworked :: Text -> Plan -> Bool
isReworked name plan = case occurrences name plan of
    _ : _ : _ -> True
    _ -> False


-- | The tag a change requires its own earlier occurrence at: @v1@ in
-- @foo [foo\@v1] …@, which is what @sqitch rework@ writes. 'Nothing' for
-- a change that is not a rework, and also for one that depends on another
-- change at a tag (@foo [bar\@v1]@).
reworkTag :: Change -> Maybe Text
reworkTag c =
    listToMaybe
        [ t
        | d <- c.dependencies
        , d.kind == Requires
        , isNothing d.project
        , OnChange n (Just t) <- [d.target]
        , n == c.name
        ]


-- | Every later occurrence of a change, in plan order.
reworks :: Plan -> [Rework]
reworks plan = go Map.empty (changes plan)
  where
    go _ [] = []
    go seen (c : cs) =
        let rest = go (Map.insert c.value.name c seen) cs
         in case Map.lookup c.value.name seen of
                Just previous -> Rework{change = c, previous, tag = reworkTag c.value} : rest
                Nothing -> rest


----------------------------------------------------------------------------------------------------
-- Building

-- | A change as @sqitch add@ writes it.
newChange :: Text -> [Dependency] -> UTCTime -> Planner -> Maybe Text -> Change
newChange name dependencies timestamp planner note =
    Change
        { operator = Nothing
        , name
        , dependencies
        , timestamp
        , planner
        , note = defaultNote <$> note
        , layout = defaultEntryLayout
        }


-- | A tag as @sqitch tag@ writes it.
newTag :: Text -> UTCTime -> Planner -> Maybe Text -> Tag
newTag name timestamp planner note =
    Tag{name, timestamp, planner, note = defaultNote <$> note, layout = defaultEntryLayout}


-- | Add lines at the end of a plan, numbering them on from its last.
-- The plan then ends with a newline.
appendItems :: [Item] -> Plan -> Plan
appendItems items plan =
    let lastLine = maybe 0 (.line) (listToMaybe (reverse plan.entries))
        new = zipWith (\n i -> Located{line = n, value = i}) [lastLine + 1 ..] items
     in Plan{entries = plan.entries <> new, finalNewline = plan.finalNewline || not (null items)}


----------------------------------------------------------------------------------------------------

located :: (Item -> Maybe a) -> Plan -> [Located a]
located f plan = mapMaybe (traverse f) plan.entries
