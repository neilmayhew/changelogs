module Changelog where

import CMark
import Data.List (sortOn)
import Data.Text (Text, unpack)
import Data.Version (Version, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

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

buildSections :: Node -> (Markdown, [Section])
buildSections (Node _ DOCUMENT docNodes) = foldr go mempty docNodes
 where
  go (Node _ (HEADING level) titleNodes) (ns, ss) =
    let (children, others) = span ((level <) . sectionLevel) ss
     in ([], Section level titleNodes ns children : others)
  go n (ns, ss) =
    (n : ns, ss)
buildSections (Node mPos typ _) = error $ "Unexpected top-level node type (" <> show typ <> ") at " <> show mPos

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

newtype Entry = Entry Markdown
  deriving (Eq, Ord, Show)

parseChangelog :: Text -> Changelog
parseChangelog = makeChangeLog . buildSections . commonmarkToNode []

makeChangeLog :: (Markdown, [Section]) -> Changelog
makeChangeLog ([], [Section 1 title [] sections]) = Changelog title $ map makeRelease sections
makeChangeLog unexpected = error $ "Unexpected Changelog input: " <> show unexpected

makeRelease :: Section -> Release
makeRelease (Section 2 title markdown subsections) =
    Release (makeVersion title) (makeEntries markdown) $ map makeSublib subsections
makeRelease unexpected = error $ "Unexpected Release parse result: " <> show unexpected

makeVersion :: Markdown -> Version
makeVersion = parseVersion' . mconcat . map nodeText
 where
  parseVersion' :: Text -> Version
  parseVersion' t = case sortOn (length . snd) . readP_to_S parseVersion . unpack $ t of
    (v, _) : _ -> v
    unexpected -> error $ "Unexpected Version parse result: " <> show unexpected

makeSublib :: Section -> Sublib
makeSublib (Section 3 title markdown []) = Sublib title $ makeEntries markdown
makeSublib unexpected = error $ "Unexpected Sublib input: " <> show unexpected

makeEntries :: Markdown -> [Entry]
makeEntries [Node _ (LIST _) entries]
  | all ((ITEM ==) . nodeType) entries = map (Entry . nodeChildren) entries
makeEntries [] = []
makeEntries unexpected = error $ "Unexpected Entries input: " <> show unexpected
