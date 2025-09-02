{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Changelog

import Data.Foldable (for_)
import Data.Text (Text, unpack)
import Options.Applicative
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import qualified System.Console.Terminal.Size as TS
import qualified Data.Text.IO as T

data Options = Options
  { optChangelogs :: [String]
  , optDirectory :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optBulletHierarchy :: Text
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optDirectory <-
                optional . strOption $
                  help "Filenames are relative to DIR"
                    <> short 'C'
                    <> long "directory"
                    <> metavar "DIR"
              optOutput <-
                optional . strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
              optBulletHierarchy <-
                strOption $
                  help "Use CHARS for the levels of bullets"
                    <> short 'b'
                    <> long "bullets"
                    <> metavar "CHARS"
                    <> value "*-+"
                    <> showDefaultWith unpack
              optChangelogs <-
                some . strArgument $
                  help "Changelog files to process"
                    <> metavar "CHANGELOG ..."
              pure Options {..}
          )
          (fullDesc <> header "Parse and lint changelog files")
      )

  for_ optChangelogs $ \fp -> do
    let
      pError e = hPutStrLn stderr $ fp <> ": " <> e
      pPrint = maybe T.putStr T.writeFile optOutput . renderChangelog optBulletHierarchy
      fp' = maybe fp (</> fp) optDirectory
    either pError pPrint =<< parseChangelogFile fp'

parseChangelogFile :: FilePath -> IO (Either String Changelog)
parseChangelogFile f = parseChangelog <$> T.readFile f
