{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

import           Data.List                          (isPrefixOf)
import           Distribution.Simple
import           Distribution.Types.HookedBuildInfo
import           System.Directory
import           System.Exit                        (ExitCode (..))
import           System.Process                     (callProcess, readProcessWithExitCode)

main = defaultMainWithHooks simpleUserHooks { preBuild = buildDuktape }

outputDir = "dukconfig/dist"
scriptFile = "duktape/tools/configure.py"
scriptArgs =
  [ "--output-directory", outputDir
  , "--source-directory", "duktape/src-input"
  , "--config-metadata", "duktape/config"
  , "--option-file", "dukconfig/config.yaml"
  ]
repoSource = "duktape/src/duktape.c"
builtSource = "dukconfig/dist/duktape.c"

buildDuktape _ _ = do
  script ← doesFileExist scriptFile
  if script then do
    createDirectoryIfMissing True outputDir
    needBuild ← filesCurrent
    if needBuild then do
      runScript
      done
    else done
  else done
  where
  done = return emptyHookedBuildInfo
  filesCurrent = do
    builtExists ← doesFileExist builtSource
    if builtExists then do
        repoTime ← getModificationTime repoSource
        builtTime ← getModificationTime builtSource
        return $ repoTime > builtTime
    else return True
  runScript =
    findPython >>= \case
        Right python →
          callProcess python (scriptFile : scriptArgs)
        Left reason → do
            putStrLn "Couldn't find Python 2 with PyYAML, which is required to configure duktape."
            putStrLn reason

-- Duketape's configure script requires python2 with PyYAML installed.
-- Check candidates "python2, python" and return one that will work or reasons why
findPython ∷ IO (Either String FilePath)
findPython =
  tryPython "python2" >>= \case
      Left reason2 →
          tryPython "python" >>= \case
              Left reason →
                  return $ Left $ concat ["python2: ", reason2, "\npython: ", reason]
              found → return found
      found → return found
  where
  tryPython exe = findExecutable exe >>= \case
                      Nothing → return $ Left "Executable not found."
                      Just path → checkPython path
  checkPython path =
      readProcessWithExitCode path ["--version"] "" >>= \case
          (ExitSuccess, "", version) → checkPrefix version path
          (ExitSuccess, version, "") → checkPrefix version path
          (ExitFailure _, out, err) → return $ Left (out ++ err)
          _ → return $ Left "Execution failed."
  checkPrefix version path
              | "Python 2" `isPrefixOf` version = checkPyYAML path
              | otherwise = return $ Left version
  checkPyYAML path =
      readProcessWithExitCode path ["-c", "import yaml"] "" >>= \case
          (ExitSuccess, "", _) → return $ Right path
          _ → return $ Left "No PyYaml installed."
