-- | Reading a @sqitch.plan@.
--
-- The grammar follows sqitch's own reader (@App::Sqitch::Plan@), which
-- is line based:
--
-- * A line of only whitespace is blank.
-- * A line whose first non-blank character is @#@ is a comment.
-- * Anywhere else, the first @#@ starts the line's note.
-- * What precedes the note is a pragma (@%name=value@), a tag
--   (@\@name timestamp planner \<email\>@) or a change
--   (@[+|-]name [deps] timestamp planner \<email\>@).
--
-- Names follow sqitch's rules ('validName'): no whitespace or @:\@#\\@,
-- neither end is punctuation (@_@ is not punctuation), the last character
-- is a word character, and the name does not end in one of @~/=%^[]@ followed by digits, which sqitch reserves
-- for references like @HEAD^2@. A change or tag being declared is held
-- to more ('validChangeName', 'validTagName'); a dependency may refer to
-- the reserved names and to change IDs.
module Sqitch.Plan.Parse (
    parsePlan,
    ParseError (..),
    renderParseError,
    parseDependency,
    validName,
    validChangeName,
    validTagName,
    isChangeId,
) where

import Control.Monad (unless, when)
import Data.Char (GeneralCategory (ConnectorPunctuation), generalCategory, isAlphaNum, isDigit, isMark, isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Sqitch.Plan.Types
import Prelude


-- | A line the parser could not read.
data ParseError = ParseError
    { line :: Int
    -- ^ 1-based.
    , message :: Text
    , source :: Text
    -- ^ The offending line, as written.
    }
    deriving stock (Eq, Show)


-- | @line N: message@ followed by the line itself.
renderParseError :: ParseError -> Text
renderParseError e = "line " <> Text.pack (show e.line) <> ": " <> e.message <> "\n    " <> e.source


-- | Read a whole plan. Every line becomes one entry, so
-- 'Sqitch.Plan.Render.render' gives the source back unchanged.
parsePlan :: Text -> Either ParseError Plan
parsePlan src
    | Text.null src = Right Plan{entries = [], finalNewline = False}
    | otherwise = do
        let finalNewline = "\n" `Text.isSuffixOf` src
            body = if finalNewline then Text.dropEnd 1 src else src
            readLine (n, l) = case parseLine l of
                Left message -> Left ParseError{line = n, message, source = l}
                Right item -> Right Located{line = n, value = item}
        entries <- traverse readLine (zip [1 ..] (Text.splitOn "\n" body))
        pure Plan{entries, finalNewline}


isBlank :: Char -> Bool
isBlank = isSpace


-- | Split off the longest suffix of characters satisfying the predicate.
spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
spanEnd p t = (Text.dropWhileEnd p t, Text.takeWhileEnd p t)


parseLine :: Text -> Either Text Item
parseLine raw
    | Text.all isBlank raw = Right (Blank raw)
    | otherwise =
        let (lead, rest) = Text.span isBlank raw
         in case Text.uncons rest of
                Just ('#', comment) -> Right (CommentItem Comment{indent = lead, text = comment})
                _ -> do
                    let (content0, notePart) = Text.breakOn "#" rest
                        (content, gap) = spanEnd isBlank content0
                        note = case Text.uncons notePart of
                            Just (_, afterNote) ->
                                let (afterHash, text) = Text.span isBlank afterNote
                                 in Just Note{gap, afterHash, text}
                            Nothing -> Nothing
                        trailing = if isJust note then "" else gap
                    case Text.uncons content of
                        Just ('%', p) -> PragmaItem <$> parsePragma lead p note trailing
                        Just ('@', t) -> TagItem <$> parseTag lead t note trailing
                        _ -> ChangeItem <$> parseChange lead content note trailing


parsePragma :: Text -> Text -> Maybe Note -> Text -> Either Text Pragma
parsePragma leading p note trailing = do
    let (afterPercent, r1) = Text.span isBlank p
        (name, r2) = Text.break (\c -> c == '=' || isBlank c) r1
        (beforeEquals, r3) = Text.span isBlank r2
        layout afterEquals = PragmaLayout{leading, afterPercent, beforeEquals, afterEquals, trailing}
    when (Text.null name) $ Left "a pragma needs a name after the %"
    case Text.uncons r3 of
        Nothing -> Right Pragma{name, value = Nothing, note, layout = layout ""}
        Just ('=', r4) ->
            let (afterEquals, value) = Text.span isBlank r4
             in Right Pragma{name, value = Just value, note, layout = layout afterEquals}
        Just _ -> Left ("expected = after the pragma name " <> name)


parseTag :: Text -> Text -> Maybe Note -> Text -> Either Text Tag
parseTag leading t note trailing = withName "tag" validTagName t \name r1 -> do
    let (afterName, r2) = Text.span isBlank r1
    when ("[" `Text.isPrefixOf` r2) $ Left "a tag cannot have dependencies"
    (timestamp, afterTimestamp, planner, beforeEmail) <- stampAndPlanner r2
    pure
        Tag
            { name
            , timestamp
            , planner
            , note
            , layout =
                EntryLayout
                    { leading
                    , afterOperator = ""
                    , afterName
                    , dependencyLayout = Nothing
                    , afterTimestamp
                    , beforeEmail
                    , trailing
                    }
            }


parseChange :: Text -> Text -> Maybe Note -> Text -> Either Text Change
parseChange leading content note trailing =
    let withOperator op r = let (s, r') = Text.span isBlank r in (Just op, s, r')
        (operator, afterOperator, r0) = case Text.uncons content of
            Just ('+', r) -> withOperator Deploy r
            Just ('-', r) -> withOperator Revert r
            _ -> (Nothing, "", content)
     in withName "change" validChangeName r0 \name r1 -> parseChangeRest leading operator afterOperator name r1 note trailing


parseChangeRest :: Text -> Maybe Operator -> Text -> Text -> Text -> Maybe Note -> Text -> Either Text Change
parseChangeRest leading operator afterOperator name r1 note trailing = do
    let (afterName, r2) = Text.span isBlank r1
    (dependencies, dependencyLayout, r3) <- case Text.uncons r2 of
        Just ('[', r) -> do
            let (inside, close) = Text.breakOn "]" r
                (afterBracket, r') = Text.span isBlank (Text.drop 1 close)
                (gaps, tokens) = splitBlanks inside
            when (Text.null close) $ Left "the dependency list has no closing ]"
            when (null tokens) $ Left "an empty dependency list; leave out the [] instead"
            deps <- traverse parseDependency tokens
            pure (deps, Just DependencyLayout{gaps, afterBracket}, r')
        _ -> pure ([], Nothing, r2)
    (timestamp, afterTimestamp, planner, beforeEmail) <- stampAndPlanner r3
    pure
        Change
            { operator
            , name
            , dependencies
            , timestamp
            , planner
            , note
            , layout =
                EntryLayout
                    { leading
                    , afterOperator
                    , afterName
                    , dependencyLayout
                    , afterTimestamp
                    , beforeEmail
                    , trailing
                    }
            }


-- | Split a change or tag name off the start of a line and hand the rest
-- on, the way sqitch's backtracking line regex does. A name may contain
-- @[@ and @]@ (@foo[bar]baz@), so the longest valid name is tried first:
-- the whole first word, then each prefix ending before a @[@ in it. The
-- first that the rest of the line parses after wins.
withName :: Text -> (Text -> Bool) -> Text -> (Text -> Text -> Either Text a) -> Either Text a
withName what valid line k =
    let word = Text.takeWhile (not . isBlank) line
        cuts = Text.length word : reverse [i | (i, c) <- zip [0 ..] (Text.unpack word), c == '[']
        attempts = [k n rest | i <- cuts, let (n, rest) = Text.splitAt i line, valid n]
        shortest = Text.takeWhile (/= '[') word
     in case attempts of
            [] -> Left case checkName what valid shortest of
                Left e -> e
                Right () -> Text.concat ["invalid ", what, " name: ", word]
            first : _ -> foldr (\a next -> either (const next) Right a) first attempts


-- | The whitespace runs and the words between them: one more run than
-- there are words, the first and last possibly empty.
splitBlanks :: Text -> ([Text], [Text])
splitBlanks s =
    let (g, r) = Text.span isBlank s
     in if Text.null r
            then ([g], [])
            else
                let (tok, r') = Text.break isBlank r
                    (gs, toks) = splitBlanks r'
                 in (g : gs, tok : toks)


-- | The timestamp, planner name and @\<email\>@ that end a change or tag.
stampAndPlanner :: Text -> Either Text (UTCTime, Text, Planner, Text)
stampAndPlanner r = do
    -- The timestamp has a fixed width, so sqitch needs no blank after it.
    let (stamp, r1) = Text.splitAt 20 r
        (afterTimestamp, r2) = Text.span isBlank r1
        (nameAndGap, r3) = Text.break (== '<') r2
        (plannerName, beforeEmail) = spanEnd isBlank nameAndGap
    when (Text.null stamp) $ Left "missing the planned timestamp"
    timestamp <- parseTimestamp stamp
    when (Text.null plannerName) $ Left "missing the planner's name"
    when (Text.null beforeEmail && not (Text.null r3)) $ Left "the planner's name and <email> need a blank between them"
    case Text.uncons r3 of
        Just ('<', r4) -> do
            let (email, r5) = Text.break (== '>') r4
            when (Text.null email) $ Left "the planner's email is empty"
            case r5 of
                ">" -> pure (timestamp, afterTimestamp, Planner{name = plannerName, email}, beforeEmail)
                "" -> Left "the planner's <email> has no closing >"
                _ -> Left ("unexpected text after the planner's email: " <> Text.drop 1 r5)
        _ -> Left "missing the planner's <email>"


parseTimestamp :: Text -> Either Text UTCTime
parseTimestamp t = case parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (Text.unpack t) of
    Just u | formatTimestamp u == t -> Right u
    _ -> Left ("not a timestamp of the form 2026-01-31T23:59:59Z: " <> t)


-- | One word of a change's @[…]@ list, as sqitch's
-- @App::Sqitch::Plan::Depend::parse@ reads it:
-- @[!][project:](change[\@tag] | \@tag | change-id)@.
parseDependency :: Text -> Either Text Dependency
parseDependency tok = do
    let (kind, r0) = case Text.uncons tok of
            Just ('!', r) -> (Conflicts, r)
            _ -> (Requires, tok)
        (project, r1) = case Text.breakOn ":" r0 of
            (p, rest) | not (Text.null rest) -> (Just p, Text.drop 1 rest)
            _ -> (Nothing, r0)
        bad :: Text -> Either Text a
        bad why = Left (Text.concat ["bad dependency ", tok, ": ", why])
        ref what n = unless (validName n) (bad (Text.concat ["invalid ", what, " name ", n]))
    mapM_ (ref "project") project
    target <-
        if isChangeId r1
            then pure (OnId r1)
            else case Text.breakOn "@" r1 of
                ("", "") -> bad "names neither a change nor a tag"
                ("", t) -> OnTag (Text.drop 1 t) <$ ref "tag" (Text.drop 1 t)
                (c, "") -> OnChange c Nothing <$ ref "change" c
                (c, t) -> OnChange c (Just (Text.drop 1 t)) <$ ref "change" c <* ref "tag" (Text.drop 1 t)
    pure Dependency{kind, project, target}


checkName :: Text -> (Text -> Bool) -> Text -> Either Text ()
checkName what valid n
    | Text.null n = Left (Text.concat ["missing the ", what, " name"])
    | valid n = Right ()
    | otherwise = Left (Text.concat ["invalid ", what, " name: ", n])


-- | Whether a change ID: exactly 40 lowercase hex digits.
isChangeId :: Text -> Bool
isChangeId t = Text.length t == 40 && Text.all isLowerHex t


isLowerHex :: Char -> Bool
isLowerHex c = isDigit c || (c >= 'a' && c <= 'f')


-- | Whether a name may be declared as a change: a 'validName' that is not
-- one of sqitch's reserved @HEAD@ and @ROOT@ and does not start with 40 hex
-- digits, which would read as a change ID.
validChangeName :: Text -> Bool
validChangeName n =
    validName n
        && n `notElem` ["HEAD", "ROOT"]
        && not (Text.length n >= 40 && Text.all isLowerHex (Text.take 40 n))


-- | Whether a name may be declared as a tag: a 'validChangeName' without a
-- @/@.
validTagName :: Text -> Bool
validTagName n = validChangeName n && not (Text.elem '/' n)


-- | Whether a name matches sqitch's name pattern. Dependencies may use
-- any such name, @HEAD@ and @ROOT@ included.
validName :: Text -> Bool
validName n = case (Text.uncons n, Text.unsnoc n) of
    (Just (first, _), Just (_, final)) ->
        not (isPunct first)
            && not (isPunct final)
            && isWordChar final
            && not (Text.any (\c -> isBlank c || c `elem` (":@#\\" :: String)) n)
            && not symbolicSuffix
    _ -> False
  where
    -- sqitch's pattern ends in @\\b@, read with Unicode semantics: the
    -- last character must be a Perl word character.
    isWordChar c = isAlphaNum c || isMark c || generalCategory c == ConnectorPunctuation
    isPunct c = c `elem` ("-!\"#$%&'()*+,./:;<=>?@[\\]^`{|}~" :: String)
    symbolicSuffix =
        let digits = Text.takeWhileEnd isDigit n
            before = Text.dropEnd (Text.length digits) n
         in not (Text.null digits) && maybe False ((`elem` ("~/=%^[]" :: String)) . snd) (Text.unsnoc before)
