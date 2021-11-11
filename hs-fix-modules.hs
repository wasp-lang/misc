{- cabal:
  build-depends: base ^>= 4.14.3.0,
                 dir-traverse ^>= 0.2.3.0,
                 filepath ^>= 1.4.2.1,
                 regex-tdfa ^>= 1.3.0.0,
                 text ^>= 1.2.0.0,
                 strict ^>= 0.4.0.0
-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_, when)
import Data.Bifunctor (second)
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.Directory.Recursive (getFilesRecursive)
import System.Environment (getArgs)
import System.FilePath (pathSeparator, splitPath, stripExtension, (</>))
import qualified System.IO.Strict
import qualified Text.Regex.TDFA as TR

-- | Run this script with @cabal run hs-fix-modules.hs [absolute_directory_paths]@, e.g. @cabal run hs-fix-modules.hs /my-hs-project/src /my-hs-project/test@`.
-- Arguments are absolute paths to directories containing the source code of your project, from which module trees start.
--
-- Script will then go through all the .hs files in those dirs and check for each of them if their Module name matches their file path.
-- E.g. if we ran @cabal run hs-fix-modules.hs /my-hs-project/src /my-hs-project/test@ and there is a file @/my-hs-project/src/Foo/Bar.hs@ that
-- has module name defined as `module Foo.Bar`, script will do nothing, as it matches its path in the @src/@ directory.
-- However if its module was instead defined as `module Bar` or `module Bar.Foo` or smth else, script would detect that this is not as it should be.
-- When the difference is detected, script updates the file to have the module name dictate by path (in this case `Foo.Bar`) and then also
-- checks all the other files looking for imports using the old module name and then replaces those with the new module name.
-- It doesn't update re-exports of modules or qualified calls though, that you need to do manually.
--
-- The main use case for using this script is when you want to do some bigger moving of code around, for example you want to prefix all your files
-- in @src/@ with `Foo`, so you create a directory Foo in there and move everything into it -> updating all the files manually to have correct module names
-- and correct imports is very tedious, so this is where this script comes in handy.

-- TODO: Rewrite whole script to use StrongPath.

-- TODO: Right now, script does not update calls to qualified imports.
--   So if we have `import qualified A.B` and there is `A.B.Something` in the code,
--   once we update the import to be `import qualified C.D`, the `A.B.Something` will not be updated, causing a compiler error.

-- TODO: Right now re-exported module statements (`module A.B` in (..) where) are not updated.

-- TODO: We could be much more efficient. Right now, for each file that has wrong module name, we read all the other files
--   in order to figure out if they need to be updated regarding imports, bringing us to O(N^2) complexity where N is number of files.
--   We could bring this down to O(N), ensuring that every file is read/written only once (max twice), by splitting the work
--   into two phases:
--     1. For each file: read it, check if module name needs to be updated: if yes, update it now (write file),
--        and remember for later pair (old name, new name).
--        Once done with this stage, we have a list of (old name, new name) pairs.
--     2. For each file: read it, check it any of its imports uses any of the old names, replace them with the corresponding new names,
--        then write the file.

main :: IO ()
main = do
  args <- getArgs
  let dirs = args

  (filesPerDir :: FilesPerDir) <- mapM getRelFilePathsInDir dirs
  let (hsFilesPerDir :: FilesPerDir) = map (second (filter isHaskellFile)) filesPerDir

  forM_ hsFilesPerDir $ \(dir, files) ->
    forM_ files (fixFileModuleName hsFilesPerDir dir)

  putStrLn "Done!"

type FilesPerDir = [(FilePath, [FilePath])]

-- | Returns (abs path to dir with no ending slash, [relative paths to files with no starting slashes]).
getRelFilePathsInDir :: FilePath -> IO (FilePath, [FilePath])
getRelFilePathsInDir absDirPath = do
  absFilePaths <- getFilesRecursive absDirPath
  return
    ( stripEndingSlashes absDirPath,
      map (stripStartingSlashes . drop (length absDirPath)) absFilePaths
    )

stripStartingSlashes :: FilePath -> FilePath
stripStartingSlashes (c : cs) | c == pathSeparator = stripStartingSlashes cs
stripStartingSlashes path = path

stripEndingSlashes :: FilePath -> FilePath
stripEndingSlashes = reverse . stripStartingSlashes . reverse

isHaskellFile :: FilePath -> Bool
isHaskellFile path = ".hs" `isSuffixOf` path

fixFileModuleName :: FilesPerDir -> FilePath -> FilePath -> IO ()
fixFileModuleName hsFilesPerDir absDir relFile = do
  let absFile = absDir </> relFile
  let expectedModuleName = getExpectedModuleNameFromHsFileRelPath relFile
  source <- System.IO.Strict.readFile absFile

  let (beforeMatch, match, afterMatch, submatches) =
        source TR.=~ ("\\b(module +)([A-Z][a-zA-Z0-9]*(\\.[A-Z][a-zA-Z0-9]*)*)" :: String) ::
          (String, String, String, [String])
  if null match
    then putStrLn $ "WARNING: Couldn't find 'module' statement in file " ++ absFile ++ ". Skipping!"
    else do
      let currentModuleName = submatches !! 1
      when (currentModuleName /= expectedModuleName) $ do
        putStrLn $ "In file " ++ absFile ++ ":"
        putStr $ "  Module name is " ++ currentModuleName ++ " but it should be " ++ expectedModuleName
        writeFile absFile $ beforeMatch ++ head submatches ++ expectedModuleName ++ afterMatch
        putStrLn " -> Updated!"
        putStrLn "  Updating imports in all the files:"
        forM_ hsFilesPerDir $ \(dir, files) ->
          forM_ files (updateFileImportsForRenamedModule currentModuleName expectedModuleName dir)

updateFileImportsForRenamedModule :: String -> String -> FilePath -> FilePath -> IO ()
updateFileImportsForRenamedModule oldModuleName newModuleName absDir relFile = do
  let absFile = absDir </> relFile
  source <- System.IO.Strict.readFile absFile
  let newSource = updateQualifiedImport $ updateRegularImport source
  when (newSource /= source) $ do
    putStr $ "    Updating imports in file " ++ absFile
    writeFile absFile newSource
    putStrLn " -> Updated!"
  where
    updateImport :: Bool -> String -> String
    updateImport isQualified src =
      let regex =
            "\\b(import +"
              ++ ( if isQualified
                     then "qualified +"
                     else ""
                 )
              ++ ")"
              ++ escapeDots oldModuleName
              ++ "([ \n\\(])"
          (beforeMatch, match, afterMatch, submatches) =
            src TR.=~ (regex :: String) :: (String, String, String, [String])
       in if null match
            then src
            else beforeMatch ++ head submatches ++ newModuleName ++ submatches !! 1 ++ afterMatch
    updateRegularImport = updateImport False
    updateQualifiedImport = updateImport True

getExpectedModuleNameFromHsFileRelPath :: FilePath -> String
getExpectedModuleNameFromHsFileRelPath path =
  let pathWithNoExt = fromMaybe (error $ "file " ++ path ++ " does not end with .hs") $ stripExtension "hs" path
      pathParts = map (stripStartingSlashes . stripEndingSlashes) $ splitPath pathWithNoExt
   in intercalate "." pathParts

escapeDots :: String -> String
escapeDots (c : cs) | c == '.' = '\\' : '.' : escapeDots cs
escapeDots (c : cs) = c : escapeDots cs
escapeDots "" = ""
