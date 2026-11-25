module ArchitectureSpec (spec) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec

spec :: Spec
spec = describe "core module boundary" $ do
  it "keeps production modules in Core or Runtime" $ do
    files <- sourceFiles "src"
    let placed path = any (`isPrefixOf` path) ["src/Baerbel/Core/", "src/Baerbel/Runtime/"]
    filter (not . placed) files `shouldBe` []
  it "keeps Core free of runtime dependencies and effect capabilities" checkCore
  it "rejects runtime and unreviewed external imports" $ do
    violations [] "import Baerbel.Runtime.HTTP" `shouldBe` ["import Baerbel.Runtime.HTTP"]
    violations [] "import qualified Data.Text.IO as Text" `shouldBe` ["import Data.Text.IO", "IO"]
    violations ["Data.Time"] "import Data.Time; clock = getCurrentTime" `shouldBe` ["getCurrentTime"]
  it "rejects effectful aliases, record fields and mutable handles" $ do
    violations [] "type Read = IO Text" `shouldBe` ["IO"]
    violations [] "data Store = Store { readState :: IO State }" `shouldBe` ["IO"]
    violations [] "data State = State (TVar Bool)" `shouldBe` ["TVar"]
  it "rejects unsafe execution and effect-generating extensions" $ do
    violations [] "x = unsafePerformIO action" `shouldBe` ["unsafePerformIO"]
    violations [] "{-# LANGUAGE TemplateHaskell #-}" `shouldBe` ["TemplateHaskell"]
  it "ignores comments and quoted text while checking qualified types" $ do
    violations [] "-- IO\n{- outer {- TVar -} Socket -}\nlabel = \"IO\"" `shouldBe` []
    violations [] "type Action = Prelude.IO Text" `shouldBe` ["IO"]

checkCore :: Expectation
checkCore = do
  allowed <- lines <$> readFile "architecture/core-imports.txt"
  files <- sourceFiles "src/Baerbel/Core"
  problems <- traverse (inspect allowed) files
  concat problems `shouldBe` []
  where
    inspect allowed path = do
      source <- readFile path
      let issues = violations allowed source
      pure (map ((path <> ": ") <>) issues)

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles directory = do
  entries <- sort <$> listDirectory directory
  let paths = map (directory </>) entries
  concat <$> traverse descend paths
  where
    descend path = doesDirectoryExist path >>= choose path
    choose path True = sourceFiles path
    choose path False = pure [path | takeExtension path == ".hs"]

violations :: [String] -> String -> [String]
violations allowed source = badImports ++ badTokens
  where
    wordsOf = tokens source
    badImports = ["import " <> name | name <- imports wordsOf, not (permitted name)]
    permitted name = "Baerbel.Core." `isPrefixOf` name || name `elem` allowed
    badTokens = filter (`elem` forbidden) wordsOf

forbidden :: [String]
forbidden =
  [ "IO", "ST", "STM", "RealWorld", "MVar", "TVar", "TMVar", "TQueue", "TBQueue"
  , "IORef", "STRef", "Async", "ThreadId", "Socket", "Handle", "Ptr", "FunPtr", "ForeignPtr", "StablePtr"
  , "MonadIO", "MonadUnliftIO", "MonadBaseControl", "MonadMask", "MonadCatch", "MonadThrow", "PrimMonad"
  , "unsafePerformIO", "unsafeDupablePerformIO", "unsafeInterleaveIO", "unsafeCoerce", "liftIO", "foreign"
  , "getCurrentTime", "getPOSIXTime", "getZonedTime", "getMonotonicTimeNSec"
  , "readFile", "writeFile", "appendFile", "putStr", "putStrLn", "print"
  , "ForeignFunctionInterface", "TemplateHaskell", "TemplateHaskellQuotes", "QuasiQuotes", "CPP"
  ]

imports :: [String] -> [String]
imports ("import" : "qualified" : rest) = imported rest
imports ("import" : rest) = imported rest
imports (_ : rest) = imports rest
imports [] = []

imported :: [String] -> [String]
imported (name : rest) = full : imports following
  where (full, following) = moduleName name rest
imported [] = []

moduleName :: String -> [String] -> (String, [String])
moduleName name ("." : next : rest) = moduleName (name <> "." <> next) rest
moduleName name rest = (name, rest)

tokens :: String -> [String]
tokens source = next (dropWhile isSpace source)
  where
    next [] = []
    next ('-' : '-' : rest) = tokens (dropWhile (/= '\n') rest)
    next ('{' : '-' : '#' : rest) = tokens rest
    next ('{' : '-' : rest) = tokens (comment 1 rest)
    next text = case lex text of
      [(word, rest)] -> word : tokens rest
      _ -> []

comment :: Int -> String -> String
comment 0 rest = rest
comment depth ('{' : '-' : rest) = comment (depth + 1) rest
comment depth ('-' : '}' : rest) = comment (depth - 1) rest
comment depth (_ : rest) = comment depth rest
comment _ [] = []
