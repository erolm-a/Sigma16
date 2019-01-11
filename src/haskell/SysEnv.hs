module SysEnv where

import Control.Exception
import Control.Monad
import Control.Monad.State
import System.IO
import System.IO.Error
import System.Directory
import System.FilePath
import System.Environment

----------------------------------------------------------------------
-- Safe input/output
----------------------------------------------------------------------

-- Perform file operations with exception handling

data IOreadResult
  = IOreadOK String
  | IOreadErr String

safeReadFile :: FilePath -> IO IOreadResult
safeReadFile p = do
  liftIO $ putStrLn ("Reading " ++ p)
  r <- try (readFile p) :: IO (Either IOError String)
  case r of
    Left e -> return (IOreadErr (show e))
    Right xs -> return (IOreadOK xs)

data IOwriteResult
  = IOwriteOK
  | IOwriteErr String

safeWriteFile :: FilePath -> String -> IO IOwriteResult
safeWriteFile p xs = do
  liftIO $ putStrLn ("Writing " ++ show (length xs) ++ " bytes to "
                     ++ p)
  r <- try (writeFile p xs) :: IO (Either IOError ())
  case r of
    Left e -> return (IOwriteErr (show e))
    Right r -> return IOwriteOK
    

----------------------------------------------------------------------
-- Finding the installation directory
----------------------------------------------------------------------

-- Find, or try to find, the location of the installation directory.
-- This is needed to locate static data files and the documentation
-- files.  There are several possible ways to find it; these depend on
-- whether the program is compiled or interpreted with ghci, and on
-- the operating system, and on the user's environment.  They are
-- tried in sequence.  If there is a command line argument this is
-- used. If no command line argument, then the executable path is
-- tried.  The execuatable path should be correct for a compiled
-- installation on Windows, or for a compiled installation on Linux
-- that has not been moved since it was compiled.  However, if ghci is
-- being used, the executable path will be the location of ghc, not
-- the location of Sigma16.  There are two problems with asking the
-- operating system where the executable is: if the program is being
-- interpreted, this will be ghci, not the actual program.  In this
-- case, "Haskell Platform" or "ghc" is likely to be on the path.
-- Also, on Linux this value is baked in at compile time, so if the
-- program has been moved the location may be wrong.

getInstDir :: IO (Maybe [FilePath])
getInstDir = do
  args <- getArgs
  if length args > 0    -- Use command line argument if present
    then return (Just (splitPath (args!!0)))
    else do             -- Otherwise use executable path
      ep <- getExecutablePath
      return (Just (splitPath (takeDirectory ep)))

----------------------------------------------------------------------
-- Read a path
----------------------------------------------------------------------

-- Given a path, try to read it from the file system

data PathReadResult
  = ReadFailed String
  | PathIsDir [FilePath]
  | PathIsFile String

-- readPath: Given a path, read it and get its contents, represented
-- as a PathReadResult.  It's possible for the file system to change
-- between any of the IO operations, so even if an existence test
-- gives True the file or directory might not exist when it's read.
-- The existence tests are made for two reasons: they will determine
-- whether to read a path as a directory or as a file, and they are
-- helpful for producing error messages.  The doesPathExist check is
-- not made; even if that were True it would not ensure that the path
-- is either a directory or a file, when those are checked.  If an
-- IOError occurs while checking for existence of a path, directory,
-- of file, this will simply be treated as a False result for the
-- existence.  If an IOError occurs while reading a directory or file,
-- the value of will be treated as an empty directory or file.

falseIfIOError :: IOError -> IO Bool
falseIfIOError _ = return False

nilIfIOError :: IOError -> IO [a]
nilIfIOError _ = return []

readPath :: FilePath -> IO PathReadResult
readPath p = do
  de <- catchIOError (doesDirectoryExist p) falseIfIOError
  case de of
    True -> do
      xs <- catchIOError (listDirectory p) nilIfIOError
      return (PathIsDir xs)
    False -> do
      fe <- catchIOError (doesFileExist p) falseIfIOError
      case fe of
        False -> return (ReadFailed ("Cannot read file " ++ p))
        True -> do
          xs <- catchIOError (readFile p) nilIfIOError
          return (PathIsFile xs)

--  pe <- catchIOError (doesPathExist p) falseIfIOError
--  case pe of
--    False -> return (ReadFailed ("Cannot read path " ++ p))
--    True -> do
