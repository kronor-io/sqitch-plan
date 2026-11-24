-- | Writing a plan back out. For any plan 'Sqitch.Plan.Parse.parsePlan'
-- accepts, @render <$> parsePlan src == Right src@.
module Sqitch.Plan.Render (
    render,
    renderItem,
    renderPragma,
    renderChange,
    renderTag,
    renderDependency,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Sqitch.Plan.Types
import Prelude


render :: Plan -> Text
render p =
    Text.intercalate "\n" (map (renderItem . (.value)) p.entries)
        <> (if p.finalNewline then "\n" else "")


-- | One line, without its newline.
renderItem :: Item -> Text
renderItem = \case
    Blank t -> t
    CommentItem c -> c.indent <> "#" <> c.text
    PragmaItem p -> renderPragma p
    ChangeItem c -> renderChange c
    TagItem t -> renderTag t


renderPragma :: Pragma -> Text
renderPragma p =
    let l = p.layout
        value = maybe "" (\v -> l.beforeEquals <> "=" <> l.afterEquals <> v) p.value
     in l.leading <> "%" <> l.afterPercent <> p.name <> value <> ending p.note l.trailing


renderChange :: Change -> Text
renderChange c =
    let l = c.layout
        operator = case c.operator of
            Nothing -> ""
            Just Deploy -> "+" <> l.afterOperator
            Just Revert -> "-" <> l.afterOperator
        deps = map renderDependency c.dependencies
        -- Sqitch rejects an empty @[]@, so a change without dependencies
        -- gets no brackets, whatever its layout says, and at least one
        -- space before its timestamp.
        dependencies = case l.dependencyLayout of
            _ | null deps -> if Text.null l.afterName then " " else l.afterName
            Nothing -> Text.concat [l.afterName, "[", Text.unwords deps, "] "]
            Just dl
                | length dl.gaps == length deps + 1 -> Text.concat [l.afterName, "[", interleave dl.gaps deps, "]", dl.afterBracket]
                | otherwise -> Text.concat [l.afterName, "[", Text.unwords deps, "]", dl.afterBracket]
     in l.leading <> operator <> c.name <> dependencies <> stampAndPlanner c.timestamp c.planner c.note l


renderTag :: Tag -> Text
renderTag t =
    let l = t.layout
     in l.leading <> "@" <> t.name <> l.afterName <> stampAndPlanner t.timestamp t.planner t.note l


-- | A dependency as it appears between the brackets. One whose names
-- contain @]@ renders, but no plan reader can parse it back: see
-- 'DependencyTarget'.
renderDependency :: Dependency -> Text
renderDependency d =
    (case d.kind of Conflicts -> "!"; Requires -> "")
        <> maybe "" (<> ":") d.project
        <> case d.target of
            OnChange c t -> c <> maybe "" ("@" <>) t
            OnTag t -> "@" <> t
            OnId i -> i


stampAndPlanner :: UTCTime -> Planner -> Maybe Note -> EntryLayout -> Text
stampAndPlanner timestamp planner note l =
    formatTimestamp timestamp
        <> l.afterTimestamp
        <> planner.name
        <> l.beforeEmail
        <> "<"
        <> planner.email
        <> ">"
        <> ending note l.trailing


ending :: Maybe Note -> Text -> Text
ending note trailing = maybe trailing (\n -> n.gap <> "#" <> n.afterHash <> n.text) note


-- | @interleave [g0, g1, g2] [a, b] == g0 <> a <> g1 <> b <> g2@.
interleave :: [Text] -> [Text] -> Text
interleave (g : gs) (x : xs) = g <> x <> interleave gs xs
interleave gs [] = mconcat gs
interleave [] xs = Text.unwords xs
