-- 2014-12-29 pandoc won't install; removed calls to it
  -- ??? 2014-12-29 pandoc removal

import Distribution.Simple
import System.Directory
import System.FilePath
-- import Text.Pandoc  -- ??? 2014-12-29 pandoc removal
-- import Text.Pandoc.Shared  -- ??? 2014-12-29 pandoc removal
-- import qualified GHC.IO.Encoding.UTF8 as U

main = defaultMainWithHooks hooks

{-  -- ??? 2014-12-29 pandoc removal
-- This is from pandoc haddoc documentation
foomarkdownToRST :: String -> String
foomarkdownToRST =
--   (writeRST defaultWriterOptions {writerReferenceLinks = True}) .
   (writeRST def {writerReferenceLinks = True}) .
--   readMarkdown defaultParserState
   readMarkdown def
-}

hooks :: UserHooks
-- hooks = simpleUserHooks {postBuild = postBuildHook}
hooks = simpleUserHooks

------------------------------------------------------------------------
-- What this Setup.hs module does...
------------------------------------------------------------------------

{- Haddock creates its documentation in the form of a set of files in
dist/doc/html/HydraGUI.  These files have names matching *.html,
*.gif, *.css, *.js, HydraGUI.haddock.  After running haddock, we have:

  dist/doc/html/HydraGUI/(files created by Haddock)

HydraGUI has much more extensive documentation, produced by pandoc
from sources in doc.  The top level file in this is index.html.  To
avoid conflicts between the primary HydraGUI documentation and the API
documentation from haddock, the following steps are taken:

  1. A list of files in dist/doc/html/HydraGUI/ is created, and named
     haddock_files

  2. A directory dist/doc/html/HydraGUI/haddock is created

  3. All the files in dist/doc/html/HydraGUI[haddock_files] are copied
     to dist/doc/html/HydraGUI/haddock

  4. All the files in dist/doc/html/HydraGUI[haddock_files] are
     removed

  5. Pandoc is run on the documentation source files, with the results
     placed in dist/doc/html/HydraGUI.  These files contain relative
     pointers (URLs) into the haddock documentation.

The result is a documentation directory in the same place Cabal
expects to find it --- dist/doc/html/HydraGUI --- which contains the
HydraGUI documentation as well as the API reference produced by
haddock.

 -}

------------------------------------------------------------------------

postBuildHook args flags packageDescription localBuildInfo =
  return ()
--  do putStrLn (take 72 (repeat '-'))
--     putStrLn "running postBuildHook"

{- The documentation sources are in doc/src, and the files that will
be used by the program will be created and placed in doc/html, with
the figures in doc/html/figures.  Make sure those directories
exist. -}

--     createDirectoryIfMissing True (joinPath ["doc", "html"])
--     createDirectoryIfMissing True (joinPath ["doc", "html", "figures"])
--     createDirectoryIfMissing True (joinPath ["doc", "html", "Sigma16"])

-- Run pandoc to build the html files from the sources

{-   -- ??? 2014-12-29 pandoc removal
     putStrLn "Running pandoc..."
--     runPandoc topTemplate secTemplate vbTemplate
     runPandoc topTemplate
     putStrLn "Pandoc finished"
-}

{- Copy the figure files into doc/html/figures.  There may be source
figure files in doc/src/figures, along with object files.Only the
object figure files will be copied. -}

{-
     putStrLn (take 72 (repeat '-'))

     figure_files <- getDirectoryContents
                       (joinPath ["system", "doc", "src", "figures"])

     putStrLn ("Figure files: " ++
                concat (map ((++"\n") . show) figure_files))

     copy_files
       (joinPath ["doc", "src", "figures"])
       (joinPath ["doc", "html", "figures"])
       figure_files
-}

-- Copy some miscellaneous files that are needed in doc/html.  These
-- include the css styles, and some text files in the main package
-- directory.
{-
     putStrLn (take 72 (repeat '-'))
     putStrLn "copy miscellaneous files"

     copyFile
       (joinPath ["doc", "src", "style.css"])
       (joinPath ["doc", "html", "style.css"])

--     copyFile
--       (joinPath ["examples", "Sigma16", "Add.asm.txt"])
--       (joinPath ["doc", "html", "Sigma16", "Add.asm.txt"])

     putStrLn (take 72 (repeat '-'))

     putStrLn "postBuildHook finished"
     return ()
-}

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

move_files src dest [] = return ()
move_files src dest (x:xs) =
  do putStrLn x
     if head x == '.' || x=="haddock" || x=="figures"
       then putStrLn ("Skipping file <" ++ x ++ ">")
       else do let srcpath = joinPath [src,x]
               let destpath = joinPath [dest,x]
               putStrLn ("Moving <" ++ srcpath ++
                         "> to <" ++ destpath ++ ">")
               renameFile srcpath destpath
     move_files src dest xs

copy_files src dest [] = return ()
copy_files src dest (x:xs) =
  do putStrLn x
     let ext = takeExtension x
     let srcpath = joinPath [src,x]
     let destpath = joinPath [dest,x]
     putStrLn ("filepath = <" ++ x ++ "> extension = <" ++ ext ++ ">")
     if head x == '.' || ext/=".png"
       then putStrLn ("Skipping file <" ++ x ++ ">")
       else do putStrLn ("Copying <" ++ srcpath ++
                         "> to <" ++ destpath ++ ">")
               copyFile srcpath destpath
     copy_files src dest xs

-- Run pandoc to convert sources (markdown) to targets (html)

{-   -- ??? 2014-12-29 pandoc removal
runPandoc topTemplate =
  do putStrLn "Building index.html"
     let infilepath = joinPath ["doc", "src", "index.txt"]
     let outfilepath = joinPath ["doc", "html", "index.html"]
     inp <- readFile infilepath
--     let pandocRepresentation = readMarkdown defaultParserState inp
     let pandocRepresentation = readMarkdown def inp
     let htmlOutput =
           writeHtmlString
--             (defaultWriterOptions
             (def
                {writerStandalone = True,
                 writerTableOfContents = True,
                 writerTemplate = topTemplate})
             pandocRepresentation
     writeFile outfilepath htmlOutput
     let pdfOutput =
           writeHtmlString
--             (defaultWriterOptions
             (def
                {writerStandalone = True,
                 writerTableOfContents = True,
                 writerTemplate = topTemplate})
             pandocRepresentation
     writeFile outfilepath htmlOutput
     putStrLn "mkHtml: finished"
-}
