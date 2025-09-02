{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelog where

import CMark
import Control.Monad ((<=<))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.List (sortOn)
import Data.Text (Text, pack, unpack)
import Data.Version (Version, parseVersion, showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Text as T

-- CMark helper functions

nodePos :: Node -> Maybe PosInfo
nodePos (Node mp _ _) = mp

nodeType :: Node -> NodeType
nodeType (Node _ ty _) = ty

nodeChildren :: Node -> [Node]
nodeChildren (Node _ _ ns) = ns

nodeText :: Node -> Text
nodeText (Node _ typ children) = mconcat $ implicitText : map nodeText children
 where
  implicitText = case typ of
    CODE t -> t
    CODE_BLOCK _ t -> t
    HTML_BLOCK t -> t
    HTML_INLINE t -> t
    TEXT t -> t
    _ -> mempty

type Markdown = [Node]

-- Grouping into sections

data Section = Section
  { sectionLevel :: Int
  , sectionTitle :: Markdown
  , sectionPreamble :: Markdown
  , sectionContent :: [Section]
  }
  deriving (Eq, Ord, Show)

buildSections :: Node -> Except String (Markdown, [Section])
buildSections (Node _ DOCUMENT docNodes) = pure $ foldr go mempty docNodes
 where
  go (Node _ (HEADING level) titleNodes) (ns, ss) =
    let (children, others) = span ((level <) . sectionLevel) ss
     in ([], Section level titleNodes ns children : others)
  go n (ns, ss) =
    (n : ns, ss)
buildSections (Node mPos typ _) = throwError $ "Unexpected top-level node type (" <> show typ <> ") at " <> show mPos

unbuildSections :: (Markdown, [Section]) -> Node
unbuildSections (md, ss) =
  Node Nothing DOCUMENT $ md <> concatMap go ss
 where
  go :: Section -> Markdown
  go Section {..} =
    Node Nothing (HEADING sectionLevel) sectionTitle :
      sectionPreamble <> concatMap go sectionContent

-- Changelogs

data Changelog = Changelog
  { changelogTitle :: Markdown
  , changelogVersions :: [Release]
  }
  deriving (Eq, Ord, Show)

data Release = Release
  { cvNumber :: Version
  , cvEntries :: [Entry]
  , cvSublibs :: [Sublib]
  }
  deriving (Eq, Ord, Show)

data Sublib = Sublib
  { slName :: Markdown
  , slEntries :: [Entry]
  }
  deriving (Eq, Ord, Show)

newtype Entry = Entry
  { unEntry :: Markdown
  }
  deriving (Eq, Ord, Show)

parseChangelog :: Text -> Either String Changelog
parseChangelog = runExcept . (makeChangeLog <=< buildSections . commonmarkToNode [])

makeChangeLog :: (Markdown, [Section]) -> Except String Changelog
makeChangeLog ([], [Section 1 title [] sections]) = Changelog title <$> traverse makeRelease sections
makeChangeLog unexpected = throwError $ "Unexpected Changelog input: " <> show unexpected

makeRelease :: Section -> Except String Release
makeRelease (Section 2 title markdown subsections) =
  Release <$> makeVersion title <*> makeEntries markdown <*> traverse makeSublib subsections
makeRelease unexpected = throwError $ "Unexpected Release parse result: " <> show unexpected

makeVersion :: Markdown -> Except String Version
makeVersion = parseVersion' . mconcat . map nodeText
 where
  parseVersion' :: Text -> Except String Version
  parseVersion' t = case sortOn (length . snd) . readP_to_S parseVersion . unpack $ t of
    (v, _) : _ -> pure v
    unexpected -> throwError $ "Unexpected Version parse result: " <> show unexpected

makeSublib :: Section -> Except String Sublib
makeSublib (Section 3 title markdown []) = Sublib title <$> makeEntries markdown
makeSublib unexpected = throwError $ "Unexpected Sublib input: " <> show unexpected

makeEntries :: Markdown -> Except String [Entry]
makeEntries [Node _ (LIST _) entries]
  | all ((ITEM ==) . nodeType) entries = pure $ map (Entry . nodeChildren) entries
makeEntries [] = pure []
makeEntries unexpected = throwError $ "Unexpected Entries input: " <> show unexpected

renderChangelog :: Text -> Changelog -> Text
renderChangelog bullets = fixMarkdownStyle bullets . nodeToCommonmark [] Nothing . unbuildSections . unmakeChangelog

unmakeChangelog :: Changelog -> (Markdown, [Section])
unmakeChangelog Changelog {..} = ([], [Section 1 changelogTitle mempty $ map unmakeRelease changelogVersions])

unmakeRelease :: Release -> Section
unmakeRelease Release {..} =
  Section 2 (textNode . pack $ showVersion cvNumber) (unmakeEntries cvEntries) (map unmakeSublib cvSublibs)
 where
  textNode t = [Node Nothing (TEXT t) []]

unmakeEntries :: [Entry] -> Markdown
unmakeEntries = (:[]) . Node Nothing (LIST listAttrs) . map (Node Nothing ITEM . unEntry)
 where
  listAttrs = ListAttributes
    { listType = BULLET_LIST
    , listTight = True
    , listStart = 0
    , listDelim = PERIOD_DELIM
    }

unmakeSublib :: Sublib -> Section
unmakeSublib Sublib {..} = Section 3 slName (unmakeEntries slEntries) []

fixMarkdownStyle :: Text -> Text -> Text
fixMarkdownStyle bullets = T.unlines . map (fixEmptyBullets . fixEscapes . fixLine) . T.lines
 where
  fixLine l =
    let
      (spaces, rest) = T.span (== ' ') l
      level = T.length spaces `div` 4
      bullet = T.index bullets (level `mod` T.length bullets)
     in
      T.replicate level "  " <> case T.uncons rest of
        Just ('-', rest') -> T.cons bullet rest'
        _ -> rest
  fixEscapes = T.replace "\\#" "#" . T.replace "\\>" ">"
  fixEmptyBullets l = if  "* " == l then "*\n" else l
