 module Main where

-- This is a main program for Sigma16, providing a GUI in a web
-- browser, using the Threepenny library to interface with the
-- browser.

-- Run main in this module, and open localhost:8023 in a browser

import Control.Concurrent
import Control.Exception
import System.IO.Error
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.List
import Data.Char
import Data.Word
import Text.Read
import System.Directory
import System.FilePath
import System.Exit 
import System.IO
import System.IO.Error
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements

import Arithmetic
import Architecture
import Common
import State
import Assembler
import Linker
import Controller
import Emulator
import SysEnv

-- There's also State.getCurrentModule, needs refactoring

getCurMod :: SystemStateRef -> UI (Maybe S16module)
getCurMod ssr = do
  p <- getAct ssr actUiState s16program
  i <- getAct ssr actUiState s16CurrentMod
  return $
    if 0 <= i && i < length p
      then Just (p !! i)
      else Nothing

getCurrentFilePath :: SystemStateRef -> UI (Maybe FilePath)
getCurrentFilePath ssr = do
  mm <- getCurMod ssr
  case mm of
    Nothing -> return Nothing
    Just m ->
      case s16modFilePath m of
        Nothing -> return Nothing
        Just fp -> return (Just fp)

setCurrentFilePath :: SystemStateRef -> FilePath -> UI ()
setCurrentFilePath ssr fpath = do
  p <- getAct ssr actUiState s16program
  i <- getAct ssr actUiState s16CurrentMod
  if 0 <= i && i < length p
    then do
      let mod = p !! i
      let mod' = mod {s16modFilePath = Just fpath}
      let p' = take i p ++ [mod'] ++ drop (i+1) p
      putAct ssr actUiState (\s -> s { s16program = p'})
      return ()
    else return ()

-- Messages
bpLongStatusError = "Syntax error in breakpoint specification"
bpLongStatusOn    = "Breakpoint is on"
bpLongStatusOff   = "Breakpoint is off"
bpShortStatusOn   = "on"
bpShortStatusOff  = "off"

validAsciiString :: String -> Bool
validAsciiString xs = and (Data.List.map f xs)
  where f c = isAscii c && isPrint c  -- is character ok?

-- Replace special html characters with the html encoding
-- Replace non-Ascii characters with message

restrictToPrintableAscii :: String -> String
restrictToPrintableAscii [] = []
restrictToPrintableAscii (c:cs) =
  htmlRep c ++ restrictToPrintableAscii cs

htmlRep c
  | c=='<'  = "&lt;"
  | c=='>'  = "&gt;"
  | c=='&'  = "&amp;"
  | c=='"'  = "&quot;"
  | not (isAscii c) = "##non-ascii character##"
  | otherwise = [c]

{-
restrictToPrintableAscii :: String -> (Bool, String)
restrictToPrintableAscii xs =
  let ls = lines xs
      ok = and (concat (Data.List.map (Data.List.map charOK) ls))
      ls' = Data.List.map (Data.List.map fixChar) ls
      charOK c = isAscii c && isPrint c
      fixChar c = if charOK c then c else ' '
  in (ok, unlines ls')
-}


-- liftst = Control.Monad.State.lift

setCurrentModule :: SystemStateRef -> Int -> Element -> UI ()
setCurrentModule ssr i edElt = do
  liftIO $ putStrLn ("setCurrent Module " ++ show i)
  putAct ssr actUiState (\s -> s {s16CurrentMod = i})
  prog <- getAct ssr actUiState s16program
  let src = if i >= 0 && i < length prog
              then case s16modAsmSrc (prog !! i) of
                     Nothing -> ""
                     Just xs -> xs
              else ""
  liftIO $ putStrLn ("setCurrentModule " ++ src)
  element edElt # set UI.value src
  return ()

-- highlightAsmErrs :: [String] -> [(Int,Int)] -> [String]
--

breakHtmlLines :: [String] -> String
breakHtmlLines = concat . Data.List.map addBr

addBr :: String -> String
addBr xs = xs ++ "<br>"

encodeTextForHtml :: String -> String
encodeTextForHtml [] = []
encodeTextForHtml (x:xs)
  | x == '<'  = "&lt;" ++ encodeTextForHtml xs
  | x == '>'  = "&gt;" ++ encodeTextForHtml xs
  | x == '&'  = "&amp;" ++ encodeTextForHtml xs
  | otherwise = x : encodeTextForHtml xs

{- working here...
refreshMem :: SystemStateRef -> StateT ArchState IO ()
refreshMem = do
  s <- get
  let xs = [showMemLocation mem a | a <- [0..hwm]]  

-- old temp kludge for initializing mem display
  let memText =  -- ??? replace this with proper refresh
        concat [ showw i ++ " " ++ showw 0 ++ "\n"
          | i <- [0 .. 2^uiMemSize-1]]
-}

----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

-- Configuring the memory.  The architecture has 2^16 locations, and
-- these are always available (i.e. the emulator always provides all
-- of these locations).  However, the GUI display is slow when all
-- 2^16 locations are shown in the memory views (this is noticable
-- when you click on the Processor tab; there is typically a three
-- second delay in showing the processor pane).  Many Sigma16 programs
-- don't need the full memory.  Therefore the GUI is configured to
-- display only a portion of the memory.

-- To do: the emulator should maintain the high water mark of memory
-- usage, and expand the display as needed.

-- The GUI displays 2 ^ uiMemSize locations. The largest value allowed
-- is 16, but the GUI responds faster with a smaller memory.  A
-- reasonable default is memsize = 12.


----------------------------------------------------------------------
-- Notes on defining operations with javascript
----------------------------------------------------------------------

-- From Threepenny haddock page:

-- ffi :: FFI a => String -> a Source #

-- Simple JavaScript FFI with string substitution.  Inspired by the
-- Fay language. https://github.com/faylang/fay/wiki

-- example :: String -> Int -> JSFunction String
-- example = ffi "$(%1).prop('checked',%2)"

-- The ffi function takes a string argument representing the
-- JavaScript code to be executed on the client. Occurrences of the
-- substrings %1 to %9 will be replaced by subequent arguments. The
-- substring %% in the original will be replaced by % (character
-- escape).

-- Note: Always specify a type signature! The types automate how
-- values are marshalled between Haskell and JavaScript. The class
-- instances for the FFI class show which conversions are supported.

-- Example:
{-
setFoobar :: JSFunction ()
setFoobar = ffi
  "document.getElementById(\"foobar\").innerHTML = \"new text\""
-}

----------------------------------------------------------------------
-- How to define and use Javascript functions
----------------------------------------------------------------------

-- Here's an example of how to define and use a global function in the
-- javascript world

-- The Javascript code to define a global function named testGlob

globFcnExperimentText :: String
globFcnExperimentText =
  " testGlob = function () { \
  \ document.getElementById(\"MiscLabel\").innerHTML = \
  \   \"Changed!\" } "

-- Running the definition code, to bring testGlob into global scope

globFcnExperiment :: JSFunction ()
globFcnExperiment = ffi globFcnExperimentText

-- A Haskell function to call the Javascript function

runGlobFcn :: JSFunction ()
runGlobFcn = ffi "testGlob();"

-----------------------------------------
globShowAsmLine :: String
globShowAsmLine =
  " showAsmLine = function () { \
  \   var elmnt = document.getElementById(\"ASMCU\"); \
  \   elmnt.scrollIntoView(); \
  \   } "

-- Running the definition code, to bring testGlob into global scope

defineShowAsmLine :: JSFunction ()
defineShowAsmLine = ffi globShowAsmLine

-- A Haskell function to call the Javascript function
-- runShowAsmLine :: JSFunction ()
-- runShowAsmLine = ffi "showAsmLine();"

-----------------------------------------
-- Using the function:  runFunction runGlobFcn

{- An experiment to see of a global variable can be defined; the trick
is to use `myvar = value', and not `var myvar = value'.

 "  MyGlobalVar = \"hello\";"

useGlobVar :: JSFunction ()
useGlobVar = ffi  "document.getElementById(\"MiscLabel\").innerHTML = MyGlobalVar;"
-}

----------------------------------------------------------------------
-- Operations defined using Javascript code
----------------------------------------------------------------------

-- setFoobar: experiment to execute Javascript in browser

setB100 :: JSFunction ()
setB100 = ffi
  "document.getElementById(\"b100\").innerHTML = \"foobaz\""

-- Set height of a div element (which may contain html)
-- elid is the element id, which is a string
-- h is the height
-- setDivHeight :: String -> Int -> JSFunction ()
setDivHeight elid h = ffi $
  "document.getElementById(\"" ++ elid ++ "\").style.height = \""
    ++ show h ++ "px\";"

----------------------------------------------------------------------
-- Some IO operations
----------------------------------------------------------------------

----------------------------------------------------------------------
-- File chooser dialogue
----------------------------------------------------------------------

-- The file chooser dialogue is normally hidden, and appears as a
-- popup when needed (editor:Open, editor:SaveAs).  Javascript code to
-- show and hide the file chooser.

-- Usage: runFunction chooserShowOpen
showFileChooser, hideFileChooser :: JSFunction ()
showFileChooser = ffi
  "document.getElementById(\"FileChooser\").style.display = \"block\""
hideFileChooser = ffi
  "document.getElementById(\"FileChooser\").style.display = \"none\""

-- The chooser can be used either to select an existing file (Open) or
-- to create a new file (SaveAs); separate buttons to specialise the
-- chooser for open or SaveAs can be shown or hidden.

data FileChooserAction = FileChooserOpen | FileChooserSaveAs
  deriving Show

chooserShowOpen,   chooserHideOpen   :: JSFunction ()
chooserShowCreate, chooserHideCreate :: JSFunction ()
chooserShowOpen = ffi
  "document.getElementById(\"ChooseFileSelect\").style.display = \"block\""
chooserHideOpen = ffi
  "document.getElementById(\"ChooseFileSelect\").style.display = \"none\""
chooserShowCreate = ffi
  "document.getElementById(\"CreateFileButton\").style.display = \"block\""
chooserHideCreate = ffi
  "document.getElementById(\"CreateFileButton\").style.display = \"none\""


showDoc, hideDoc :: JSFunction ()
showDoc = ffi
  "document.getElementById(\"DocSection\").style.display = \"block\""
hideDoc = ffi
  "document.getElementById(\"DocSection\").style.display = \"none\""

-- Given a filename, return the full path for that file

pathForFile :: SystemStateRef -> FilePath -> UI (Maybe FilePath)
pathForFile ssr fname = do
  mp <- getCurrentPath ssr
  return $ case mp of
    Nothing -> Nothing     -- don't know directory, so fail
    Just p -> Just (p </> fname)  -- attach filename to directory

-- Given a list of filenames xs, build a panel of buttons to select
-- one of them and put it in GUI element e.  Each button performs
-- action a when clicked.

buildFileButtonPanel
  :: SystemStateRef
  -> (Int -> FilePath -> UI ())
  -> Element
  -> [FilePath]
  -> UI ()
buildFileButtonPanel ssr f e xs = do
--  liftIO $ putStrLn ("buildFileButtonPanel " ++ show xs)
-- Delete existing buttons so they can be garbage collected
--  oldbs <- element e # Graphics.UI.Threepenny.Core.get UI.children
-- Create buttons for the file names and put them in the element
--  bs <- mkFileButtons f (zip [0..] xs)
  bs <- mkFileButtons f (zip [0..] (sort xs))
  element e # set UI.children bs
  return ()

mkFileButtons
  :: (Int -> String -> UI ())
  -> [(Int,String)]
  -> UI [Element]
mkFileButtons f [] = return []
mkFileButtons f ((i,x):xs) = do
  b <- UI.button # set UI.text x
  on UI.click b $ const $ do
    liftIO $ putStrLn ("file/mod button clicked: ("
                        ++ show i ++ ") " ++ show x)
    f i x
  bs <- mkFileButtons f xs
  return (b:bs)


-- Set the provisional current working path to p.  If p is unreadable
-- display a message.  If p is a directory, show its contents in
-- dirWidget, and insert its contents into the directory file
-- selection combo box.  If p is a file, show its contents in
-- fileWidget and save that value in the ui environment (so we can
-- check whether it has changed if the file is closed).

setProvisionalPath
  :: SystemStateRef  -- ssr = access to state of the application
--  -> Element       -- fcDirCombo = combo box for directory contents
  -> Element         -- dirButtons = div for buttons for dir contents
  -> Element         -- dirPreview = text area for directory listing
  -> Element         -- fcPathEntry = input box for file path
  -> Element         -- fileWidget = text area for file contents
  -> FilePath        -- p = path to read
  -> UI ()

setProvisionalPath
    ssr
--    dirCombo
    dirButtons
    preview
    pathEntry fileText p = do
  liftIO $ putStrLn ("setProvisionalPath " ++ p)
  x <- liftIO $ readPath p
  case x of
    ReadFailed msg -> do
      liftIO $ putStrLn ("setProvisionalPath: cannot read path " ++ p)
      element preview
        # set UI.text ("Cannot read path " ++ p)
      return ()
    PathIsDir fs -> do
--      liftIO $ putStrLn ("setProvPath have dir, fs = " ++ show fs)
      putEdProvisionalPath ssr (Just p)          -- save path
      element pathEntry # set UI.text p    -- display path
      putEdProvisionalDirContents ssr (Just fs) -- save ls
      let f i x = setProvisionalPath ssr
                    dirButtons preview
                    pathEntry fileText (p </> x)
      buildFileButtonPanel ssr f dirButtons fs
      return ()
    PathIsFile txt -> do
      putEdProvisionalPath ssr (Just p)      -- save path
      element pathEntry # set UI.text p   -- display path
      putEdProvisionalText ssr (Just txt)  -- save text
      element preview # set UI.text txt             -- display text
      return ()

--                  dirCombo
--      element preview
--        # set UI.text ("dir = " ++ show fs)    -- show contents
--    fileOptions <- mkOpts fs                       -- prepare combo
--    element dirCombo # set children fileOptions -- set combo box

----------------------------------------------------------------------
-- Functions for module selection dialogue
----------------------------------------------------------------------

-- Similar to file chooser dialogue

showModSel, hideModSel :: JSFunction ()
showModSel = ffi
  "document.getElementById(\"ModSelPopup\").style.display = \"block\""
hideModSel = ffi
  "document.getElementById(\"ModSelPopup\").style.display = \"none\""

buildModSelButtonPanel
  :: SystemStateRef   -- access to system state
  -> Element          -- element on which to place buttons
  -> [String]         -- text to put in the buttons
  -> Element          -- editor text area
  -> UI ()
buildModSelButtonPanel ssr e xs edElt = do
  liftIO $ putStrLn ("buildModSelButtonPanel " ++ show xs)
  let f i x = do
        liftIO $ putStrLn ("mod panel f " ++ show i ++ "," ++ show x)
        setCurrentModule ssr i edElt
        runFunction hideModSel
        return ()
  bs <- mkFileButtons f (zip [0..] xs)
  element e # set UI.children bs
  return ()

----------------------------------------------------------------------
-- Breakpoint dialogue
----------------------------------------------------------------------

-- The breakpoint dialogue is normally hidden, and appears as a popup
-- when needed.  Javascript code to show and hide the file chooser.

-- Usage: runFunction showBreakpoint
showBreakpoint, hideBreakpoint :: JSFunction ()
showBreakpoint = ffi
  "document.getElementById(\"BreakpointPopup\").style.display = \"block\""
hideBreakpoint = ffi
  "document.getElementById(\"BreakpointPopup\").style.display = \"none\""


getBreakpointText :: JSFunction String
getBreakpointText = ffi
  "document.getElementById(\"BreakpointTextArea\").value"


----------------------------------------
-- Get contents of text area
----------------------------------------

getEnterFileNameInput :: JSFunction String
getEnterFileNameInput = ffi
  "document.getElementById(\"EnterFileNameInput\").value"

getCreateDirInput :: JSFunction String
getCreateDirInput = ffi
  "document.getElementById(\"CreateDirNameInput\").value"


getFcPathEntryText :: JSFunction String
getFcPathEntryText = ffi
  "document.getElementById(\"ProvisionalPathText\").value"

-- getFilePathText :: JSFunction String
-- getFilePathText = ffi
--   "document.getElementById(\"FilePathInput\").value"


getEditorText :: JSFunction String
getEditorText = ffi
  "document.getElementById(\"EdTextArea\").value"

getTrText :: JSFunction String
getTrText = ffi
  "document.getElementById(\"TrInTextArea\").value"

getInputBufferText ::  JSFunction String
getInputBufferText = ffi
  "document.getElementById(\"ProcInputBuffer\").value"

----------------------------------------
-- Get window height
----------------------------------------

-- Obtain the current height of the container, so the vertical sizes
-- of textboxes can be adjusted to fill the vertical space

jsGetHeight :: JSFunction Int
jsGetHeight = ffi "window.innerHeight"

-- UI operation to get window height
getCurrentWindowHeight :: UI Int
getCurrentWindowHeight = do
  result <- callFunction jsGetHeight
  return result

----------------------------------------
-- Window sizes
----------------------------------------

-- To set height of a textarea e, set rows
-- To set height of a div e, set e.style.height (using Javascript)

textAreaLineHeight :: Int
textAreaLineHeight = 13

-- Default PxPerLine = 16 :: Int defined in init UI state.  Height of
-- text in pixels, for scrolling; experiment to determine the vluae:
-- 13? 15? 16?

edTextAreaHeightAbove, asmTextAreaHeightAbove,
  lnkTextAreaHeightAbove, logTextAreaHeightAbove,
  devTextAreaHeightAbove :: Int

edTextAreaHeightAbove  = 110
asmTextAreaHeightAbove = 110
lnkTextAreaHeightAbove = 110
logTextAreaHeightAbove = 110
devTextAreaHeightAbove = 110
procListingHeightAbove = 475

setHeights edTextArea lnkTextArea logTextArea devTextArea = do
-- text area heights: use elements
    liftIO $ putStrLn "Set heights: Auto adjust text height"
    autoAdjustTextArea edTextArea  edTextAreaHeightAbove
    autoAdjustTextArea lnkTextArea lnkTextAreaHeightAbove
    autoAdjustTextArea logTextArea logTextAreaHeightAbove
    autoAdjustTextArea devTextArea devTextAreaHeightAbove
-- div html area heights; use ID strings rather than elements
    liftIO $ putStrLn "set heights asmTextArea"
    autoAdjustDivHeight "AsmTextArea" asmTextAreaHeightAbove
    liftIO $ putStrLn "set heights procAsmListing"
    autoAdjustDivHeight "ProcAsmListing" procListingHeightAbove

-- The height parameters are measured in pixels

adjustTextArea :: Element -> Int -> Int -> UI ()
-- t is a textArea to be adjusted to fill space vertically
-- k1 = size in px of region above t
-- h is height in px of the pane
-- r is calculated to be the number of rows for text area
-- adjustTextArea txtarea k1 k2 h = do
adjustTextArea txtarea k1 h = do
  let r = ((h - k1) `Prelude.div` textAreaLineHeight) - 1
  liftIO $ putStrLn ("adjustTextArea r = " ++ show r)
  element txtarea # set UI.rows (show r)
  return ()

autoAdjustTextArea :: Element -> Int -> UI ()
autoAdjustTextArea txtarea k1 = do
  h <- getCurrentWindowHeight
  adjustTextArea txtarea k1 h

-- setDivHeights asmTextArea = do
--    autoAdjustDivArea asmTextArea asmTextAreaHeightAbove

adjustDivArea e k1 h = do
  let r = h - k1
  liftIO $ putStrLn ("adjustDivArea r = " ++ show r)
  element e # set UI.height r
  return ()


-- Adjust height of a div
-- (not used) e is an element whose height should be adjusted
-- elid is a string giving element ID
-- k1 is height above the element
-- h is height of the window
-- r is target height in pixels
adjustDivHeight elid k1 h = do
  let r = h - k1
  liftIO $ putStrLn
    ("adjustDivHeight"
      ++ "  h=" ++ show h
      ++ "  k1=" ++ show k1
      ++ "  r= " ++ show r)
--  element e # set UI.height r   -- doesn't work
  runFunction (setDivHeight elid r)
  return ()

-- autoAdjustDivHeight :: Element -> Int -> UI ()
autoAdjustDivHeight :: String -> Int -> UI ()
autoAdjustDivHeight elid k1 = do
  h <- getCurrentWindowHeight
  adjustDivHeight elid k1 h

----------------------------------------
-- Scrolling a text area
----------------------------------------

-- tryMemScroll: scroll the selected text area by a specified number
-- of pixels down from the top

tryMemScroll :: JSFunction ()
tryMemScroll = ffi
  "document.getElementById(\"memView1\").scrollTop = 5000"

scrollAsmLine :: JSFunction ()
scrollAsmLine = ffi $
  "document.getElementById(\"ASMCU\").scrollIntoView()"
{-
  "document.getElementById(\""
    ++ idString
    ++ "\").scrollTop = "
    ++ show nPixels
    ++ ";"
-}

scrollFromTop :: String -> Int -> JSFunction ()
scrollFromTop idString nPixels = ffi $
  "document.getElementById(\""
    ++ idString
    ++ "\").scrollTop = "
    ++ show nPixels
    ++ ";"

----------------------------------------
-- Show or hide popup windows
----------------------------------------

-- To do, abstract these functions

showPopup, hidePopup :: JSFunction ()
showPopup = ffi
  "document.getElementById(\"PopupMessage\").style.display = \"block\""
hidePopup = ffi
  "document.getElementById(\"PopupMessage\").style.display = \"none\""


----------------------------------------
-- Tabbed panes
----------------------------------------

hideWelcomePane :: JSFunction ()
hideWelcomePane = ffi
  "document.getElementById(\"WelcomePane\").style.display = \"none\""
showWelcomePane :: JSFunction ()
showWelcomePane = ffi
  "document.getElementById(\"WelcomePane\").style.display = \"block\""

hideEditorPane :: JSFunction ()
hideEditorPane = ffi
  "document.getElementById(\"EditorPane\").style.display = \"none\""
showEditorPane :: JSFunction ()
showEditorPane = ffi
  "document.getElementById(\"EditorPane\").style.display = \"block\""

hideAsmPane :: JSFunction ()
hideAsmPane = ffi
  "document.getElementById(\"AsmPane\").style.display = \"none\""
showAsmPane :: JSFunction ()
showAsmPane = ffi
  "document.getElementById(\"AsmPane\").style.display = \"block\""

hideLinkerPane :: JSFunction ()
hideLinkerPane = ffi
  "document.getElementById(\"LinkerPane\").style.display = \"none\""
showLinkerPane :: JSFunction ()
showLinkerPane = ffi
  "document.getElementById(\"LinkerPane\").style.display = \"block\""

hideProcPane :: JSFunction ()
hideProcPane = ffi
  "document.getElementById(\"ProcPane\").style.display = \"none\""
showProcPane :: JSFunction ()
showProcPane = ffi
  "document.getElementById(\"ProcPane\").style.display = \"block\""

hideTracePane :: JSFunction ()
hideTracePane = ffi
  "document.getElementById(\"TracePane\").style.display = \"none\""
showTracePane :: JSFunction ()
showTracePane = ffi
  "document.getElementById(\"TracePane\").style.display = \"block\""

hideCircuitPane :: JSFunction ()
hideCircuitPane = ffi
  "document.getElementById(\"CircuitPane\").style.display = \"none\""
showCircuitPane :: JSFunction ()
showCircuitPane = ffi
  "document.getElementById(\"CircuitPane\").style.display = \"block\""

hideLogPane :: JSFunction ()
hideLogPane = ffi
  "document.getElementById(\"LogPane\").style.display = \"none\""
showLogPane :: JSFunction ()
showLogPane = ffi
  "document.getElementById(\"LogPane\").style.display = \"block\""

hideDevPane :: JSFunction ()
hideDevPane = ffi
  "document.getElementById(\"DevPane\").style.display = \"none\""
showDevPane :: JSFunction ()
showDevPane = ffi
  "document.getElementById(\"DevPane\").style.display = \"block\""

----------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------


--------------------------------------------------
-- Popups
--------------------------------------------------

-- Test popup
-- e should be the popup message payload
showMessageInPopup :: Element -> String -> UI ()
showMessageInPopup e xs = do
  element e # set UI.html xs
  runFunction showPopup

-- Popup that returns a result of type a

--------------------------------------------------
-- Check editor to see if there is unsaved text
--------------------------------------------------

checkUnsavedText :: SystemStateRef -> UI ()
checkUnsavedText ssr = return ()

----------------------------------------------------------------------
-- Main program
----------------------------------------------------------------------

-- main is defined for convenient testing, but should not be exported.
-- Alternative user interfaces, such as the text user interface, also
-- define main programs.

main :: IO ()
main = mainBrowserGUI

-- The real main function is mainBrowserGUI

mainBrowserGUI :: IO ()
mainBrowserGUI = do
  hSetBuffering stdout LineBuffering
  currentDir <- getCurrentDirectory
  maybeInstallationDir <- getInstDir
  case maybeInstallationDir of
    Nothing -> error
      "Cannot find installation directory.\n\
      \*** You can specify it on command line; see user guide"
    Just instDir -> do
      let instPath = joinPath instDir
      putStrLn ("Installation path = " ++ instPath)
      let staticPath = instPath </> "datafiles" </> "static"
      putStrLn ("Current directory = " ++ currentDir)
      putStrLn ("Static data directory = " ++ staticPath)

      startGUI defaultConfig
        { jsPort    = Just 8023
        , jsStatic  = Just staticPath
        , jsWindowReloadOnDisconnect = True
        } (setup instDir)

----------------------------------------------------------------------
-- Set up the GUI
----------------------------------------------------------------------

setup :: [FilePath] -> Window -> UI ()
setup s16dir window = do
  ssr <- initialSystemState
  liftIO $ putStrLn "setup..."
  return window
    # set UI.title "Sigma16"
  UI.addStyleSheet window "BrowserGUI.css"
--  runFunction globFcnExperiment
--  liftIO $ UI.debug window "This is a message to console..."

-------------------------------------------------------
-- Define key components early to get them in scope
-------------------------------------------------------

-- These widgets are defined here so they will be in scope as
-- functions that refer to them are defined

-- Some text areas appear at the bottom of a pane, and should grow or
-- shrink vertically to fill the vertical space available.  That way,
-- these text areas will have a scroll bar allowing all the text to be
-- visible, while the area above the text will still be visible.  The
-- vertical size of the text areas is set dynamically by
-- adjustTextArea.

  emuBPstatus <- UI.div
    # set UI.text bpShortStatusOff
    # set UI.class_ "RegVal"

  pauseIORef <- liftIO $ newIORef False

  edTextArea <- UI.textarea
    # set UI.id_ "EdTextArea"
    # set UI.class_ "BottomTextArea"

  asmTextArea <- UI.div
    # set UI.id_ "AsmTextArea"
    # set UI.class_ "BottomTextArea"

  lnkTextArea <- UI.textarea
    # set UI.id_ "LnkTextArea"
    # set UI.class_ "BottomTextArea"

  logTextArea <- UI.textarea
    # set UI.id_ "LogTextArea"
    # set UI.class_ "BottomTextArea"

  breakpointTextArea <- UI.textarea
    # set UI.id_ "BreakpointTextArea"
    # set UI.class_ "BottomTextArea"
    # set UI.rows "20"

  devTextArea <- UI.textarea
    # set UI.id_ "DevTextArea"
    # set UI.class_ "BottomTextArea"

-- Set up buffers and handlers for Input/Output

  procIOtext <- UI.textarea
    # set UI.id_ "ProcIOtext"
    # set UI.class_ "ClassTextArea"
    # set UI.rows "10"
    # set UI.cols "50"
{-
--  procIOtext <- UI.div
  procIOtext <- UI.textarea
    # set UI.id_ "ProcIOtext"
    # set UI.class_ "ClassTextArea"
-}

--    # set UI.html "<em>This is initial</em>stuff now bigger"
  procInputBuffer <- UI.textarea
    # set UI.id_ "ProcInputBuffer"
    # set UI.class_ "ClassTextArea"
    # set UI.rows "3"
    # set UI.cols "50"
--    # set UI.html "bla bla init text"
--    # set UI.rows "3"

-------------------------------------------------------
-- Define parameters
-------------------------------------------------------

-- These should be made settable through the GUI

  let uiMemSize = 16 :: Int
      -- memory has 2 ^ uiMemSize locations

-------------------------------------------------------
-- Define global Javascript functions
-------------------------------------------------------

  runFunction defineShowAsmLine  -- define Javascript function

-------------------------------------------------------
-- Define popup boxes and MVars for messages
-------------------------------------------------------

  popupMessageCloseButton <- UI.button
    # set UI.text "Close this popup"
    # set UI.id_ "PopupMessageCloseButton"
  on UI.click popupMessageCloseButton $ const $ do
       runFunction hidePopup
  popupMessagePayload <- UI.p
    # set UI.html "this is the message payload"
    # set UI.id_ "PopupMessagePayload"
  popupMessage <- UI.div
    # set UI.id_ "PopupMessage"
    # set UI.class_ "PopupBox"
    #+ [ element popupMessageCloseButton
       , element popupMessagePayload
       ]

--  on UI.click popupMessageCancelButton $ const $ do
--       runFunction hidePopup

-------------------------------------------------------
-- File chooser  popup
-------------------------------------------------------

  fileChooserHeader <- UI.h1 # set UI.text "File Chooser"

  dirNavigationHeader <- UI.h2 # set UI.text "Navigation"

  dirDocumentsButton <- UI.button
    # set UI.text "Documents"
  dirHomeButton <- UI.button
    # set UI.text "Home"
  dirExamplesButton <- UI.button
    # set UI.text "Examples"
  dirSysLibButton <- UI.button
    # set UI.text "System library"
  dirUpButton <- UI.button
    # set UI.text "Up to parent"

  fcDirButtons <- UI.div
    # set UI.id_ "FileDirDisplay"
--    #+ [element dirB1, element dirB2, element dirB3, element dirB4,
--        element dirB5]

{-
  pickDirSelectionButton <- UI.button
    # set UI.text "Select (choose from combo box, then click)"
  fcDirCombo <- UI.select
-}

  createDirButton <- UI.button
    # set UI.text "Create directory (enter name, then click)"
    # set UI.id_ "CreateDirButton"
  enterDirNameInput <- UI.input
    # set UI.type_ "text"
    # set UI.value ""
    # set UI.id_ "CreateDirNameInput"
  enterPathHeader <- UI.h2 # set UI.text "Filename and path"
  setProvisionalPathLabel <- UI.label # set UI.text "Current path:"
  fcPathEntry <- UI.textarea
    # set UI.id_ "ProvisionalPathText"
--    # set UI.rows "3"

  setProvisionalPathButton <- UI.button
    # set UI.text "Set file path"

  previewHeader <- UI.h2 # set UI.text "Preview directory or file"
  fcPreview <- UI.textarea
    # set UI.id_ "FileChooserPreviewArea"
    # set UI.text ""
  finishHeader <- UI.h2 # set UI.text "Finish and close"
  fcCancelButton <- UI.button
     # set UI.text "Cancel"
    # set UI.id_ "ChooseFileCancel"

  fcWriteButton <- UI.button
    # set UI.text "Save as (enter name, then click)"
    # set UI.id_ "CreateFileButton"
  fcReadButton <- UI.button
    # set UI.text "Open selected file"
    # set UI.id_ "ChooseFileSelect"
  enterFileNameInput <- UI.input
    # set UI.type_ "text"
    # set UI.value ""
    # set UI.id_ "EnterFileNameInput"

-- Several actions for the file chooser are defined as functions that
-- require several arguments.  The following shortcut definitions
-- provide the arguments, which are in scope here, to shorten the code
-- later on.

  let fcSetPath = setProvisionalPath
        ssr                   -- system state reference
        fcDirButtons            -- div to hold buttons for dir entries
        fcPreview            -- text area to preview selection
        fcPathEntry       -- entry to show or edit path
        edTextArea            -- text area to place text file contents
        -- apply this to p :: FilePath = path to set as current

--      fcDirCombo           -- combo box to choose dir entry
--  let fcSetDir = setDirectory ssr fcDirCombo
--        fcPreview fcPathEntry edTextArea
  

-- Actions for the buttons

  on UI.click dirDocumentsButton $ const $ do
       liftIO $ putStrLn "Documents directory clicked"
       d <- liftIO getUserDocumentsDirectory
       fcSetPath d

  on UI.click dirHomeButton $ const $ do
       liftIO $ putStrLn "Home directory clicked"
       d <- liftIO getHomeDirectory
       fcSetPath d

  on UI.click dirExamplesButton $ const $ do
       liftIO $ putStrLn "Examples directory clicked"
       fcSetPath
--       setProvisionalPath ssr fcDirCombo -- selectedDirInput
--         fcPreview fcPathEntry edTextArea
         (joinPath s16dir </> "datafiles" </> "programs"
            </> "Examples")

  on UI.click dirSysLibButton $ const $ do
       liftIO $ putStrLn "SysLib directory clicked"
       fcSetPath
--       setProvisionalPath ssr fcDirCombo -- selectedDirInput
--         fcPreview fcPathEntry edTextArea
         (joinPath s16dir </> "datafiles" </> "programs"
            </> "Library")

  on UI.click dirUpButton $ const $ do
-- get current directory and setProvisionalPath to its parent
       liftIO $ putStrLn "directory up clicked"
       md <- getEdProvisionalPath ssr
       liftIO $ putStrLn ("up md = " ++ show md)
       case md of
         Nothing -> return ()
         Just d -> do
           fcSetPath (takeDirectory d)

  on UI.click createDirButton $ const $ do
       liftIO $ putStrLn "Create directory clicked"
       md <- getEdProvisionalPath ssr
       liftIO $ putStrLn ("up md = " ++ show md)
       case md of
         Nothing -> return ()
         Just d -> do
           xs <- callFunction getCreateDirInput
           liftIO $ putStrLn ("Creating directory: " ++ xs)
           let newdir = d </> xs
           liftIO $ putStrLn ("Creating path: " ++ newdir)
           liftIO $ createDirectoryIfMissing True newdir
           fcSetPath newdir




{-
  on UI.click pickDirSelectionButton $ const $ do
       liftIO $ putStrLn "pickDirSelection clicked"
       mn <- UI.get UI.selection fcDirCombo
       liftIO $ putStrLn ("pickDirSelection mn = " ++ show mn)
       case mn of
         Nothing -> return ()
         Just i -> do
           md <- getEdProvisionalPath ssr
           liftIO $ putStrLn ("pickDirSelection md = " ++ show md)
           case md of
             Nothing -> return ()
             Just d -> do
               dirLs <- getEdProvisionalDirContents ssr
               liftIO $ putStrLn ("Pick, dirLs = " ++ show dirLs)
               case dirLs of
                 Nothing -> return ()
                 Just xs -> do
                   liftIO $ putStrLn ("dir select "
                               ++ show i ++ show xs)
                   fcSetPath (d </> (xs!!i)) -- ??? make safe
-}

  on UI.click setProvisionalPathButton $ const $ do
       liftIO $ putStrLn "Set file path clicked"
       xs <- callFunction getFcPathEntryText
       liftIO $ putStrLn ("Set file: " ++ xs)
       fcSetPath xs

  on UI.click fcCancelButton $ const $ do
       liftIO $ putStrLn "Choose source file: cancel"
       runFunction hideFileChooser

  on UI.click fcReadButton $ const $ do
       liftIO $ putStrLn "FC read file"
       mp <- getEdProvisionalPath ssr
       case mp of
         Just p -> do
           putCurrentPath ssr (Just p)
           r <- liftIO $ SysEnv.safeReadFile p
           case r of
             IOreadErr e -> do
               liftIO $ putStrLn ("Error reading file " ++ p)
       -- provide popup to indicate error
               return ()
             IOreadOK txt -> do
--               element edTextArea # set UI.text txt
               element edTextArea # set UI.value txt
--             liftIO $ putStrLn ("fcRead just set edText to " ++ txt)
               --       xs <- callFunction getEditorText
  -- create new empty module and put the file text in it             
               let m = emptyModule
                     { s16modAsmSrc = Just txt
                     , s16modFilePath = Just p
                     }
               newModule ssr
               putCurrentModule ssr m
--               liftIO $ putStrLn "fc read, just did putCurrentMod:"
--               liftIO $ putStrLn (show m)
               runFunction hideFileChooser
               return ()
         Nothing -> do
           liftIO $ putStrLn "FC: no path to read"
      -- provide popup
           return ()

  on UI.click fcWriteButton $ const $ do
       liftIO $ putStrLn "FC Write: save as"
       mp <- getEdProvisionalPath ssr
       case mp of
         Just p -> do
           -- check that the path is ok
           -- ???
           -- get file name, make path to save text in
           fname <- callFunction getEnterFileNameInput
           let fpath = normalise (p </> fname)
           xs <- callFunction getEditorText
           r <- liftIO $ safeWriteFile fpath xs
           case r of
             IOwriteOK -> do
               putCurrentPath ssr (Just fpath)
  -- create new empty module and put the file text in it             
               let m = emptyModule
                     { s16modAsmSrc = Just xs
                     , s16modFilePath = Just fpath
                     }
               newModule ssr
               putCurrentModule ssr m

               runFunction hideFileChooser
               return ()
             IOwriteErr msg -> do
               liftIO $ putStrLn ("Error while saving file: " ++ msg)
               return ()
         Nothing -> do
           liftIO $ putStrLn "Need a path for saving file"
           return ()

  fileChooser <- UI.div
    # set UI.id_ "FileChooser"
    # set UI.class_ "PopupBox"
    #+ [ element fileChooserHeader

       , element dirNavigationHeader
       , element dirDocumentsButton
       , element dirHomeButton
       , element dirExamplesButton
       , element dirSysLibButton
       , element dirUpButton
--       , br
--       , element pickDirSelectionButton
--       , element fcDirCombo
       , br
       , element fcDirButtons
       , br
       , element createDirButton
       , element enterDirNameInput

--       , element enterPathHeader
       , element fcPathEntry
       , element setProvisionalPathButton

       , element finishHeader
       , element fcCancelButton
       , element fcReadButton
       , element fcWriteButton
       , element enterFileNameInput
       , br
       , element previewHeader
       , element fcPreview
       ]

-------------------------------------------------------
-- Module selection popup
-------------------------------------------------------

  modSelHeader <- UI.h1 # set UI.text "Select module"
  modSelCancelButton <- UI.button
    # set UI.text "Cancel"
  on UI.click modSelCancelButton $ const $ do
       liftIO $ putStrLn "Select module: cancel"
       runFunction hideModSel
  modSelButtonPanel <- UI.div
    # set UI.id_ "ModSelButtons"
  modSelPopup <- UI.div
    # set UI.id_ "ModSelPopup"
    # set UI.class_ "PopupBox"
    #+ [ element modSelHeader
       , element modSelCancelButton
       , element modSelButtonPanel
       ]

-------------------------------------------------------
-- Welcome pane
-------------------------------------------------------

-- Layout of welcome pane

  welcomeTextArea <- UI.div
    # set UI.id_ "IdWelcomeTextArea"
    # set UI.class_ "ClassTextArea"

  homePane <- UI.div
    # set UI.id_ "WelcomePane"
--    # set UI.class_ "WelcomePane"
    # set UI.class_ "SystemPane"
    #+ [element welcomeTextArea]

-------------------------------------------------------
-- Editor pane
-------------------------------------------------------

-- Editor layout. Uses edTextArea which is defined earlier to get it
-- in scope

-- File control

  edNewFileButton <- UI.button
    # set UI.text "New"
    # set UI.id_ "NewFile"
  edOpenFileButton <- UI.button
    # set UI.text "Open"
    # set UI.id_ "OpenFile"
  edRefreshButton <- UI.button
    # set UI.text "Refresh"
    # set UI.id_ "RefreshFile"
  edClearButton <- UI.button
    # set UI.text "Clear"
    # set UI.id_ "EdClear"
  edSaveAsButton <- UI.button
    # set UI.text "Save as"
    # set UI.id_ "SaveAs"
  edSaveButton <- UI.button
    # set UI.text "Save"
    # set UI.id_ "Save"
  edCloseButton <- UI.button
    # set UI.text "Close"
    # set UI.id_ "Close"
  edModSelButton <- UI.button
    # set UI.text "Select module"
    # set UI.id_ "ModSelButton"
  edExampleButton <- UI.button
    # set UI.text "Example"
    # set UI.id_ "IdEdExampleButton"
    # set UI.class_ "ClassButton"

  editorPane <- UI.div
    # set UI.id_ "EditorPane"
--    # set UI.class_ "EditorPane"
    # set UI.class_ "SystemPane"
    #+ [ element edNewFileButton
       , element edOpenFileButton
       , element edRefreshButton
       , element edSaveAsButton
       , element edSaveButton
       , element edCloseButton
       , element edModSelButton
       , element edExampleButton
       , element edClearButton
       , element edTextArea
       ]

-- Editor behavior

  on UI.click edNewFileButton $ const $ do
    liftIO $ putStrLn "Editor New File button"
--    element edTextArea # set UI.text ""
    element edTextArea # set UI.value ""
    let m = emptyModule
              { s16modName = Just "Anonymous"
              , s16modAsmSrc = Just ""
              , s16modFilePath = Nothing
              }
    i <- getAct ssr actUiState s16CurrentMod
    prog <- getAct ssr actUiState s16program
    let n = length prog
    putAct ssr actUiState (\s -> s { s16CurrentMod = n
                                   , s16program = prog ++ [m]
                                   })

  on UI.click edOpenFileButton $ const $ do
       liftIO $ putStrLn "Open File clicked"
  -- Set user documents directory as initial directory       
       d <- liftIO getUserDocumentsDirectory
       fcSetPath d
--       element edTextArea # set UI.text ""
       element edTextArea # set UI.value ""
       runFunction chooserShowOpen
       runFunction chooserHideCreate
       runFunction showFileChooser

  on UI.click edRefreshButton $ const $ do
    liftIO $ putStrLn "Editor: Refresh"
--    element edTextArea # set UI.text ""
--    element edTextArea # set UI.value ""
    mCurModule <- getCurrentModule ssr
    case mCurModule of
      Nothing -> liftIO $ putStrLn "ed refresh: have no module"
      Just m ->
        case s16modFilePath m of
          Nothing -> liftIO $ putStrLn
                       "ed refresh: module has no path"
          Just p -> do
            liftIO  $ putStrLn $ "ed refresh: path = " ++ p

  on UI.click edClearButton $ const $ do
    liftIO $ putStrLn "Editor: Clear"
    element edTextArea # set UI.value ""

  on UI.click edSaveAsButton $ const $ do
       liftIO $ putStrLn "Save As clicked"
  -- Set user documents directory as initial directory       
       d <- liftIO getUserDocumentsDirectory
       fcSetPath d
       runFunction chooserShowCreate
       runFunction chooserHideOpen
       runFunction showFileChooser

  on UI.click edSaveButton $ const $ do
       liftIO $ putStrLn "Editor: Save"
       mfp <- getCurrentFilePath ssr
       case mfp of
         Nothing -> do -- refactor, same as save-as
           liftIO $ putStrLn "Save: no path, revert to SaveAs"
           d <- liftIO getUserDocumentsDirectory
           fcSetPath d
           runFunction chooserShowCreate
           runFunction chooserHideOpen
           runFunction showFileChooser
         Just fpath -> do
           liftIO $ putStrLn ("Save fpath = " ++ fpath)
           xs <- callFunction getEditorText
           r <- liftIO $ safeWriteFile fpath xs
           case r of  -- see alsoh onClick fcWriteButton
             IOwriteOK -> return ()
             IOwriteErr msg -> do
               liftIO $ putStrLn ("Error while saving file: " ++ msg)
               return ()



  on UI.click edCloseButton $ const $ do
    liftIO $ putStrLn "Editor: Close"
    i <- getAct ssr actUiState s16CurrentMod
    prog <- getAct ssr actUiState s16program
    if 0 <= i && i < length prog
      then do
        let prog' = take i prog ++ drop (i+1) prog
        putAct ssr actUiState (\s -> s { s16CurrentMod = 0
                                       , s16program = prog'
                                       })
        let mod0txt = case s16modAsmSrc (prog' !! 0) of
              Nothing -> ""
              Just xs -> xs
        element edTextArea # set UI.value mod0txt
        return ()
      else return ()


  on UI.click edModSelButton $ const $ do
       liftIO $ putStrLn "Select module button clicked"
--       let modNames = ["dog", "cat", "tiger"]
       s16prog <- getAct ssr actUiState s16program
       let xs = Data.List.map identS16Module s16prog
       buildModSelButtonPanel ssr modSelButtonPanel xs edTextArea
       runFunction showModSel
       liftIO $ putStrLn "mod sel just ran showModSel"
--       liftIO $ putStrLn ("Select module: " ++ show i)

  on UI.click edExampleButton $ const $ do
    liftIO $ putStrLn "Editor Example button"
    let examplePath = joinPath s16dir
          </> "datafiles" </> "programs" </> "Examples"
          </> "Simple" </> "Add.asm.txt"
    liftIO $ putStrLn ("example path =" ++ show examplePath)
    ys <- liftIO $ readFile examplePath
    let m = emptyModule
          { s16modAsmSrc = Just ys
          , s16modName = Just "Example"
          , s16modFilePath= Just examplePath
          }
--    newModule ssr
--    putCurrentModule ssr m
    p <- getAct ssr actUiState s16program
    let p' = p ++ [m]
    let i = length p
    putAct ssr actUiState
      (\s -> s {s16program = p', s16CurrentMod = i})
--    element edTextArea # set UI.text ys
    element edTextArea # set UI.value ys
    return ()

-- Example file, should it exist? have name and dir??
--    s16modName = Just "Example"
--                        , s16modDir  = Just examplePath

-------------------------------------------------------
-- Assembler pane
-------------------------------------------------------

-- Assembler layout

  assembleButton <- UI.button
    # set UI.text "Assemble"
    # set UI.id_ "AssembleButton"
    # set UI.class_ "ClassButton"

  asmPane <- UI.div
    # set UI.id_ "AsmPane"
--    # set UI.class_ "AsmPane"
    # set UI.class_ "SystemPane"
    #+ [ element assembleButton
       , element asmTextArea
       ]

-- Assembler behavior

  on UI.click assembleButton $ const $ do
       liftIO $ putStrLn "assemble clicked"
       element asmTextArea # set UI.html ""
-- Clear it so if assembly is slow, user will know when it's done
       xs1 <- callFunction getEditorText
       let xs = restrictToPrintableAscii xs1
       case validAsciiString xs1 of
         True -> liftIO $ putStrLn "editor text is valid Ascii"
         False -> liftIO $ putStrLn "editor text is not valid Ascii"
--       liftIO $ putStrLn xs  -- print source text
--       liftIO $ putStrLn "assemble button, just printed source"
       mCurModule <- getCurrentModule ssr
--       liftIO (putStrLn (take 50 (repeat '*'))) -- print module
--       liftIO (putStrLn (show mCurModule))
       case mCurModule of
         Nothing -> do -- make editor text be the current module
           return ()
         Just m -> do
           liftIO $ putStrLn "assemble, have module"
           let mod' = assemble (m { s16modAsmSrc = Just xs })
           let md = s16modMetadata mod'
--           liftIO $ putStrLn ("metadata = " ++ show md)
--           let listing = "<pre>" ++
--                   breakHtmlLines (getListing mod') ++ "</pre>"
           let listing = "<pre>" ++
                    concat (getListing mod' ++ ["</pre>"])
           element asmTextArea # set UI.html listing
--           element asmTextArea # set UI.html "elephant banana"
--           element asmTextArea # set text (concat listing)
--           element asmTextArea # set value (concat listing)
--           liftIO $ putStrLn ("ctlAsm lst = " ++ concat listing)
           putCurrentModule ssr mod'
           liftIO $ putStrLn "assemble button action finishing"
           return ()

-------------------------------------------------------
-- Linker pane
-------------------------------------------------------

-- Linker layout

  lnkShowModulesButton <- UI.button
    # set UI.text "Modules"
    # set UI.id_ "IdLnkModulesButton"
    # set UI.class_ "ClassButton"

  lnkLinkButton <- UI.button
    # set UI.text "Link"
    # set UI.id_ "IdLnkLinkButton"
    # set UI.class_ "ClassButton"

  linkerPane <- UI.div
    # set UI.id_ "LinkerPane"
--    # set UI.class_ "LinkerPane"
    # set UI.class_ "SystemPane"
    #+ [ element lnkShowModulesButton
       , element lnkLinkButton
       , element lnkTextArea
       ]

-- Linker behavior

  on UI.click lnkShowModulesButton $ const $ do
    liftIO $ putStrLn "Show modules clicked"
    xs <- showProgram ssr
    element lnkTextArea # set text xs

-------------------------------------------------------
-- Processor pane
-------------------------------------------------------


-------------------------------------------------------
-- Breakpoint dialogue popup
-------------------------------------------------------

  breakpointHeader <- UI.h1 # set UI.text "Breakpoint"
  setBreakpointButton <- UI.button
    # set UI.text "Set breakpoint"
  removeBreakpointButton <- UI.button
    # set UI.text "Remove breakpoint"
  closeBreakpointButton <- UI.button
    # set UI.text "Close"
  bpLongStatus <- UI.div
    # set UI.id_ "BPLongstatus"
    # set UI.html bpLongStatusOff
  breakpointDialogue <- UI.div
    # set UI.id_ "BreakpointPopup"
    # set UI.class_ "PopupBox"
    #+ [ element breakpointHeader
       , element setBreakpointButton
       , element removeBreakpointButton
       , element closeBreakpointButton
       , element bpLongStatus
       , element breakpointTextArea
       ]

  on UI.click setBreakpointButton $ const $ do
    liftIO $ putStrLn "breakpoint: set"
    xs <- callFunction getBreakpointText
    liftIO $ putStrLn ("set break text = " ++ xs)
    let mbp = readMaybe xs :: Maybe BPbool
    case mbp of
      Nothing -> do
        liftIO $ putStrLn ("Cannot parse breakpoint " ++ xs)
        putAct ssr actUiState
          (\a -> a {uiBreakpoint = Nothing})
        element emuBPstatus # set UI.text bpShortStatusOff
        element bpLongStatus # set UI.html bpLongStatusError
      Just bp -> do
        liftIO $ putStrLn ("Setting breakpoint " ++ xs)
        putAct ssr actUiState (\a -> a {uiBreakpoint = Just bp})
        element emuBPstatus # set UI.text bpShortStatusOn
        element bpLongStatus # set UI.html bpLongStatusOn
    return ()

  on UI.click removeBreakpointButton $ const $ do
    liftIO $ putStrLn "breakpoint: remove"
    putAct ssr actUiState
      (\a -> a {uiBreakpoint = Nothing})
    element emuBPstatus # set UI.text bpShortStatusOff
    element bpLongStatus # set UI.html bpLongStatusOff

  on UI.click closeBreakpointButton $ const $ do
    liftIO $ putStrLn "breakpoint: close"
    runFunction hideBreakpoint


----------------------------------------
-- Instruction decode
----------------------------------------

  decodeLabel <- UI.label
    # set UI.text "Instruction Decode"
    # set UI.class_ "ProcSectionLabel"
    # set UI.id_ "InstructionDecodeLabel"
  opLabel <- UI.div # set UI.text "op"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdOpLabel"
  fmtLabel <- UI.div # set UI.text "fmt"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdFmtLabel"
  argsLabel <- UI.div # set UI.text "args"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdArgsLabel"
  ccLabel <-  UI.div # set UI.text "cc"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdCcLabel"
  eff1Label <- UI.div # set UI.text "effect"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdEff1Label"
  eff2Label <- UI.div
    # set UI.text "effect"
    # set UI.class_ "DecLabel"
    # set UI.id_ "IdEff2Label"
  opVal <- UI.div # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdOpVal"
  fmtVal <- UI.div # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdFmtVal"
  argsVal <- UI.div # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdArgsVal"
  ccVal <- UI.div # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdCcVal"
  eff1Val <- UI.div
    # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdEff1Val"
  eff2Val <- UI.div
    # set UI.text ""
    # set UI.class_ "DecVal"
    # set UI.id_ "IdEff2Val"

  decodeWrapper <- UI.div
    # set UI.class_ "DecodeWrapper"
    #+ [ element decodeLabel
       , element opLabel,   element opVal
       , element argsLabel, element argsVal
       , element ccLabel,   element ccVal
       , element fmtLabel,  element fmtVal
       , element eff1Label, element eff1Val
       , element eff2Label, element eff2Val
       ]

----------------------------------------
-- Control registers
----------------------------------------

  ctlRegLabel <- UI.div
    # set UI.text "Instruction"
    # set UI.class_ "ProcSectionLabel"
    # set UI.id_ "CtlRegLabel"
  pcLabel <- UI.div
    # set UI.text "pc"
    # set UI.id_ "pcLbl"
    # set UI.class_ "RegLabel"
  pcValue <- UI.div
    # set UI.text "02b6"
    # set UI.id_ "pcVal"
    # set UI.class_ "RegVal"
  irLabel <- UI.div
    # set UI.text "ir"
    # set UI.id_ "irLbl"
    # set UI.class_ "RegLabel"
  irValue <- UI.div
    # set UI.text "02b6"
    # set UI.id_ "irVal"
    # set UI.class_ "RegVal"
  adrLabel <- UI.div
    # set UI.text "adr"
    # set UI.id_ "adrLbl"
    # set UI.class_ "RegLabel"
  adrValue <- UI.div
    # set UI.text "02b6"
    # set UI.id_ "adrVal"
    # set UI.class_ "RegVal"
  datLabel <- UI.div
    # set UI.text "dat"
    # set UI.id_ "datLbl"
    # set UI.class_ "RegLabel"
  datValue <- UI.div
    # set UI.text "02b6"
    # set UI.id_ "datVal"
    # set UI.class_ "RegVal"
  intSavePCLabel <- UI.div
    # set UI.text "savepc"
    # set UI.class_ "RegLabel"
  intSavePCValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"

  ctlRegWrapper <- UI.div
    # set UI.class_ "CtlRegWrapper"
    # set UI.id_ "InstrRegWrapper"
    #+ [ element ctlRegLabel,
         element pcLabel,  element pcValue,
         element irLabel,  element irValue,
         element adrLabel, element adrValue,
         element datLabel, element datValue,
         element intSavePCLabel,    element intSavePCValue

       ]

----------------------------------------
-- System
----------------------------------------

-- Interrupts

  intLabel <- UI.div
    # set UI.text "Interrupt"
    # set UI.class_ "ProcSectionLabel"
    # set UI.id_ "IntLabel"
  intProcStateLabel <- UI.div
    # set UI.text "state"
    # set UI.class_ "RegLabel"
  intProcStateValue <- UI.div
    # set UI.text "sys"
    # set UI.class_ "RegVal"
  intEnableLabel <- UI.div
    # set UI.text "enable"
    # set UI.class_ "RegLabel"
  intEnableValue <- UI.div
    # set UI.text "0"
    # set UI.class_ "RegVal"
  intMaskLabel <- UI.div
    # set UI.text "mask"
    # set UI.class_ "RegLabel"
  intMaskValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
  intReqLabel <- UI.div
    # set UI.text "req"
    # set UI.class_ "RegLabel"
  intReqValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
  intHandleLabel <- UI.div
    # set UI.text "handle"
    # set UI.class_ "RegLabel"
  intHandleValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"

-- Segmentation

  segLabel <- UI.div
    # set UI.text "Segment"
    # set UI.class_ "ProcSectionLabel"
    # set UI.id_ "SegLabel"
  segEnableLabel <- UI.div
    # set UI.text "enable"
    # set UI.class_ "RegLabel"
    # set UI.id_ "SegEnableLabel"
  segEnableValue <- UI.div
    # set UI.text "0"
    # set UI.class_ "RegVal"
    # set UI.id_ "SegEnableVal"
  pSegALabel <- UI.div
    # set UI.text "psa"
    # set UI.class_ "RegLabel"
    # set UI.id_ "PSALabel"
  pSegAValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
    # set UI.id_ "PSAVal"
  pSegLLabel <- UI.div
    # set UI.text "psl"
    # set UI.class_ "RegLabel"
    # set UI.id_ "PSLLabel"
  pSegLValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
    # set UI.id_ "PSLVal"
  dSegALabel <- UI.div
    # set UI.text "dsa"
    # set UI.class_ "RegLabel"
    # set UI.id_ "DSALabel"
  dSegAValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
    # set UI.id_ "DSAVal"
  dSegLLabel <- UI.div
    # set UI.text "dsl"
    # set UI.class_ "RegLabel"
    # set UI.id_ "DSLLabel"
  dSegLValue <- UI.div
    # set UI.text "0000"
    # set UI.class_ "RegVal"
    # set UI.id_ "DSLVal"

  segWrapper <- UI.div
    # set UI.class_ "CtlRegWrapper"
    # set UI.id_ "SegWrapper"
    #+ [ element segLabel,
         element segEnableLabel,   element segEnableValue,
         element pSegALabel,       element pSegAValue,
         element pSegLLabel,       element pSegLValue,
         element dSegALabel,       element dSegAValue,
         element dSegLLabel,       element dSegLValue
       ]
  intWrapper <- UI.div
    # set UI.class_ "CtlRegWrapper"
    # set UI.id_ "IntWrapper"
    #+ [ element intLabel,
         element intEnableLabel,    element intEnableValue,
         element intProcStateLabel, element intProcStateValue,
         element intMaskLabel,      element intMaskValue,
         element intReqLabel,       element intReqValue,
         element intHandleLabel,    element intHandleValue
       ]

----------------------------------------
-- Emulator control
----------------------------------------

  emuLabel <- UI.div
    # set UI.text "Emulator"
    # set UI.class_ "ProcSectionLabel"
    # set UI.id_ "EmuLabel"
  emuStatusLabel <- UI.div
    # set UI.text "status"
    # set UI.class_ "RegLabel"
    # set UI.id_ "EmuStatusLabel"
  emuStatusValue <- UI.div
    # set UI.html ""
    # set UI.class_ "RegVal"
    # set UI.id_ "EmuStatusVal"
  emuICountLabel <- UI.div
    # set UI.text "count"
    # set UI.class_ "RegLabel"
    # set UI.id_ "emuICountLabel"
  emuICountValue <- UI.div
    # set UI.text "0"
    # set UI.class_ "RegVal"
    # set UI.id_ "emuICountValue"
  emuBPlabel <- UI.div
    # set UI.text "break"
    # set UI.class_ "RegLabel"
  emuWrapper <- UI.div
    # set UI.class_ "CtlRegWrapper"
    # set UI.id_ "EmuWrapper"
    #+ [ element emuLabel
       , element emuStatusLabel,  element emuStatusValue
       , element emuICountLabel, element emuICountValue
       , element emuBPlabel, element emuBPstatus
       ]
----------------------------------------
-- Register file
----------------------------------------

  regFileElts <- mkRegFile 0

  regFileGrid <- grid (mkRegFileGrid regFileElts)
    # set UI.class_ "RegFile"
    # set UI.id_ "RegFile"

----------------------------------------
-- Memory
----------------------------------------

  let memText =  -- ??? replace this with proper refresh
        concat [ showw i ++ " " ++ showw 0 ++ "\n"
          | i <- [0 .. 2^uiMemSize-1]]

  memView1 <- UI.div
    # set UI.class_ "MemView"
    # set UI.id_ "MemView1"
    # set UI.html memText

  memView2 <- UI.div
    # set UI.class_ "MemView"
    # set UI.id_ "MemView2"
    # set UI.html memText

  memViews <- grid [[element memView1, element memView2]]
    # set UI.id_ "MemViews"

  memSection <- UI.div
    #+ [element memViews]
  
----------------------------------------
-- Layout of processor pane
----------------------------------------

  procResetButton <- UI.button
    # set UI.text "Reset"
    # set UI.id_ "IdProcResetButton"
    # set UI.class_ "ClassButton"

  procRefreshButton <- UI.button
    # set UI.text "Refresh"
    # set UI.id_ "IdProcRefreshButton"
    # set UI.class_ "ClassButton"

  procBootButton <- UI.button
    # set UI.text "Boot"
    # set UI.id_ "IdProcBootButton"
    # set UI.class_ "ClassButton"

  procStepButton <- UI.button
    # set UI.text "Step"
    # set UI.id_ "IdProcStepButton"
    # set UI.class_ "ClassButton"

  procBreakpointButton <- UI.button
    # set UI.text "Breakpoint"
    # set UI.id_ "IdProcBreakpointButton"
    # set UI.class_ "ClassButton"

  procRunDisplayButton <- UI.button
    # set UI.text "Run Display"
    # set UI.id_ "IdProcRunDisplayButton"
    # set UI.class_ "ClassButton"

  procRunButton <- UI.button
    # set UI.text "Run"
    # set UI.id_ "IdProcRunButton"
    # set UI.class_ "ClassButton"

  procPauseButton <- UI.button
    # set UI.text "Pause"
    # set UI.id_ "IdProcPauseButton"
    # set UI.class_ "ClassButton"

  procButtons <- grid
    [ [element procResetButton, element procRefreshButton,
       element procBootButton,
       element procStepButton, element procBreakpointButton,
       element procRunDisplayButton,
       element procRunButton, element procPauseButton] ]

  procLeftTop <- grid
    [[element ctlRegWrapper, element segWrapper,
      element intWrapper, element emuWrapper]]
  procLeftBottom <- grid
    [[element decodeWrapper]]
  procLeft <- grid
    [ [element procLeftTop]
    , [element procLeftBottom]
    ]


{-
  procIOsection <- grid
  procIOsection <- UI.div
    #+ [ [element procIOtext]
       , [element procInputBuffer]
       ]
    # set UI.id_ "ProcIOsection"
    # set UI.class_ "DecodeWrapper"  -- ???????????
-}

  procOutputLabel <- UI.div
    # set UI.text "Output"
    # set UI.class_ "CenterLabel"
  procInputLabel <- UI.div
    # set UI.text "Input"
    # set UI.class_ "CenterLabel"
  procIOwrapper <- UI.div
    # set UI.id_ "IOWrapper"
    #+ [ element procOutputLabel
       , element procIOtext
       , element procInputBuffer
       , element procInputLabel
       ]
  let procIOsection = procIOwrapper

  procCenterPanel <- grid
    [[element procLeft,
      element regFileGrid,
      element memSection,
      element procIOsection]]
    # set UI.id_ "ProcCenterPanel"

  procAsmListing <- UI.div
    # set UI.id_ "ProcAsmListing"

  on UI.click procBreakpointButton $ const $ do
    liftIO $ putStrLn "Breakpoint clicked"
    runFunction showBreakpoint
  
  on UI.click procResetButton $ const $ do
    liftIO $ putStrLn "Reset clicked"
    doReset ssr procIOtext procInputBuffer

  on UI.click procRefreshButton $ const $ do
    liftIO $ putStrLn "Processor Refresh"
    refreshDisplay ssr
    return ()

  on UI.click procBootButton $ const $ do
    liftIO $ putStrLn "Processor Boot"
    doReset ssr procIOtext procInputBuffer
    mCurModule <- getCurrentModule ssr
    case mCurModule of
      Nothing -> return ()
      Just m -> do
        let f = ignoreEffect
        liftIO $ putStrLn "boot, have module"
        let mobj = s16modObjCode m
        case mobj of
          Nothing -> do
            liftIO $ putStrLn "No object code, cannot boot"
            return ()
          Just obj -> do
            s <- liftIO $ readIORef ssr
            ast1 <- liftIO $ readIORef (archState s)
            (r,ast2) <- runStateT
              (readLoadModule f 0 (snd (parseLoadModule obj))) ast1
            liftIO $ writeIORef (archState s)
                       (ast2 {astProcStatus = Ready})
            let asmListing =
                  "<pre>" : (Data.List.map (++" ")
                    (Data.List.map encodeTextForHtml (getListing m)))
                    ++ ["</pre>"]
            putAct ssr actEmuState
              (\es -> es {
                          emuAsmListing = asmListing,
                          highlightedAsmLine = Nothing})
            element procAsmListing
              # set UI.html (concat asmListing)
            refreshDisplay ssr -- display object code in memory
            return ()

  on UI.click procStepButton $ const $ do
    liftIO $ putStrLn "processor step button"
    checkDoStep ssr
    s <- liftIO $ readIORef ssr
    ast <- liftIO $ readIORef (archState s)
    processInstructionEffects ssr ast
--    liftIO $ setProcStatus ssr Paused
    refreshProcStatus ssr ast
    refreshInstrCount ssr ast
    flushCallBuffer

  on UI.click procRunButton $ const $ do
    liftIO $ putStrLn "Processor Run button"
    runMultiStep ssr pauseIORef StepFast

  on UI.click procRunDisplayButton $ const $ do
    liftIO $ putStrLn "Processor Run Display button"
    runMultiStep ssr pauseIORef (StepSlow 1000000)

  on UI.click procPauseButton $ const $ do
    liftIO $ putStrLn "Processor Pause clicked"
    liftIO $ writeIORef pauseIORef True

------------------------------------------------------------
-- Processor pane
------------------------------------------------------------

  procPane <- UI.div
    # set UI.id_ "ProcPane"
    # set UI.class_ "SystemPane"
    #+ [ element procButtons
       , element procCenterPanel
       , element procAsmListing
       ]

-------------------------------------------------------
-- Trace pane
-------------------------------------------------------

  traceButton1 <- UI.button
    # set UI.text "trace button 1"

  tracePane <- UI.div
    # set UI.id_ "TracePane"
    # set UI.class_ "SystemPane"
    #+ [ element traceButton1
       ]

-------------------------------------------------------
-- Circuit pane
-------------------------------------------------------

  circuitButton1 <- UI.button
    # set UI.text "circuit button 1"
  circuitButton2 <- UI.button
    # set UI.text "circuit button 2"
  circuitButton3 <- UI.button
    # set UI.text "circuit button 3"

  dirB1 <- UI.button  # set UI.text ".."
  dirB2 <- UI.button  # set UI.text "file1"
  dirB3 <- UI.button  # set UI.text "fileZ.txt"
  dirB4 <- UI.button  # set UI.text "prog.asm.txt"
  dirB5 <- UI.button  # set UI.text "prog.obj.txt"

  circuitPane <- UI.div
    # set UI.id_ "CircuitPane"
    # set UI.class_ "SystemPane"
    #+ [ element circuitButton1
       , element circuitButton2
       , element circuitButton3
       ]

-------------------------------------------------------
-- Dev pane
-------------------------------------------------------

  devAbutton <- UI.button
    # set UI.text "TrA" # set UI.id_ "TrA"
  devBbutton <- UI.button
    # set UI.text "Set text area height"
    # set UI.id_ "TrB"
  devCbutton <- UI.button
    # set UI.text "Get window height"
    # set UI.id_ "TrC"
  devDbutton <- UI.button
    # set UI.text "TrD"
    # set UI.id_ "TrD"

  devInTextArea <- UI.textarea
    # set UI.id_ "TrInTextArea"
    # set UI.rows "1"

  devOutTextArea <- UI.textarea
    # set UI.id_ "TrOutTextArea"
    # set UI.rows "3"

  on UI.click devAbutton $ const $ do
    liftIO $ putStrLn "Tr A button"
    element devOutTextArea
      # set UI.text "Tr A button pressed"

  on UI.click devBbutton $ const $ do
    liftIO $ putStrLn "Tr B resize edtext"
    inptext <- callFunction getTrText
    let x = case readMaybe inptext of
              Nothing -> 100
              Just n -> n
    element devOutTextArea
      # set UI.text
         ("Tr B button pressed <" ++ inptext ++ "> "
          ++ show (x, x+1, x*10))
    adjustTextArea edTextArea
      edTextAreaHeightAbove
      -- textAreaLineHeight
      x
    liftIO $ putStrLn "Tr B resize edtext finished"

  on UI.click devCbutton $ const $ do
    liftIO $ putStrLn "Tr C button"
    h <- getCurrentWindowHeight
    liftIO $ putStrLn ("Tr Get window height = " ++ show h)
    element devOutTextArea
      # set UI.text ("current window height = <" ++ show h ++ ">")

  autoAdjustTextHeightButton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "Height"
  let readjustHeights = 
        setHeights edTextArea lnkTextArea logTextArea devTextArea
  on UI.click autoAdjustTextHeightButton $ const $ do
    readjustHeights

  topTestAbutton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "TestA"
  topTestBbutton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "TestB"

  on UI.click topTestBbutton $ const $ do
    liftIO $ putStrLn "TestB button clicked"
    element asmTextArea
      # set UI.html "test b <b>clicked</b>, inserting"

  hideDocButton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "Hide Doc"
  on UI.click hideDocButton $ const $ do
    liftIO $ putStrLn "Hide Doc button"
--    runFunction hideDoc
  showDocButton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "Show Doc"
  on UI.click showDocButton $ const $ do
    liftIO $ putStrLn "Show Doc button"
--    runFunction showDoc

  mainTestButton1  <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "Show popup"
  on UI.click mainTestButton1 $ const $ do
       liftIO $ putStrLn "main test button 1"
       showMessageInPopup popupMessagePayload
         "mainTestButton1 clicked, initiate popup "

  devPane <- UI.div
    # set UI.id_ "DevPane"
    # set UI.class_ "SystemPane"
    #+ [ element devAbutton
       , element devBbutton
       , element devCbutton
       , element devDbutton
       , br
       , element showDocButton
       , element hideDocButton
       , br
       , element autoAdjustTextHeightButton
       , element mainTestButton1
       , element topTestAbutton
       , element topTestBbutton

       , br
       , element devInTextArea
       , br
       , element devOutTextArea
       , element devTextArea
       ]

-------------------------------------------------------
-- Set up the tabbed panes
-------------------------------------------------------

  homeButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Welcome"
  on UI.click homeButton $ const $ do setWelcomePane


  editorButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Editor"
  on UI.click editorButton $ const $ do setEditorPane

  assemblerButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Assembler"
  on UI.click assemblerButton $ const $ do setAsmPane

  linkerButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Linker"
  on UI.click linkerButton $ const $ do setLinkerPane

  processorButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Processor"
  on UI.click processorButton $ const $ do setProcPane

  traceButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Trace"
  on UI.click traceButton $ const $ do setTracePane

  circuitButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Circuit"
  on UI.click circuitButton $ const $ do setCircuitPane

  devButton <- UI.button
    # set UI.class_ "tablinks"
    # set UI.text "Dev"
  on UI.click devButton $ const $ do setDevPane

  quitButton <- UI.button
    # set UI.class_ "top-control-button"
    # set UI.text "Quit"
  on UI.click quitButton $ const $ do
    liftIO $ putStrLn "Quit button"
    liftIO exitSuccess


-- Need to register resize event 
--  on UI.onresize window $ const $ do
--    liftIO $ putStrLn "Window resized"

  tabbedButtons <- UI.div
    # set UI.class_ "tab"
    #+ [ element homeButton
       , element editorButton
       , element assemblerButton
       , element linkerButton
       , element processorButton
       , element traceButton
       , element circuitButton
       , element devButton
       , element quitButton
       ]

----------------------------------------
-- Initialise system state
----------------------------------------
  s <- liftIO $ readIORef ssr
  
  liftIO $ putMVar (actUiState s) $ UiState
    { currentPath          = Nothing
    , edProvisionalPath      = Nothing
    , edText               = Nothing
    , edProvisionalText    = Nothing
    , edProvisionalDirContents  = Nothing
    , edTextLastSaved      = Nothing
    , s16program           = initS16program
    , s16CurrentMod        = 0
    , uiBreakpoint         = Nothing
    , textInput            = Nothing
    , pxPerLine            = 18 -- default pixels per text line
--    , pxPerLine            = 18.1 -- default pixels per text line
    }

  let astSetCount xs = do
        element emuICountValue # set UI.text xs
--        liftIO $ putStrLn ("astSetCount " ++ xs)
        return ()
  let astWrite xs = do
--        element procIOtext # set UI.html xs
        element procIOtext # set UI.value xs
--        liftIO $ putStrLn ("astWrite " ++ xs)
        return ()
  let astReadInput = do
        xs <- callFunction getInputBufferText
        return xs
  let astSetInput xs = do
        element procInputBuffer # set UI.value xs
        return ()
-- A reset operation puts the architecture into initArchState'
-- which incorporates the I/O operations
  let initArchState' = initArchState
        { astWriteOperation     = astWrite
        , astReadOperation      = astReadInput
        , astSetInputOperation  = astSetInput
        , astSetInstrCount      = astSetCount
        }

  liftIO $ putMVar (actEmuState s) $ EmuState
    { procLoadModule    = Nothing
--    , archState         = initArchState'
    , resetArchState    = initArchState'
--    , procStatus        = Reset
    , procStatusDisplay = emuStatusValue
    , instrCountDisplay = emuICountValue
--    , ctlPause          = pauseIORef
--    , memHighWaterMark  = 2^12
    , memHighWaterMark  = 2^14
    , pcDisplay         = (pcLabel,pcValue)
    , irDisplay         = (irLabel,irValue)
    , adrDisplay        = (adrLabel,adrValue)
    , datDisplay        = (datLabel,datValue)
    , opDisplay         = opVal
    , argsDisplay       = argsVal
    , ccDisplay         = ccVal
    , fmtDisplay        = fmtVal
    , eff1Display       = eff1Val
    , eff2Display       = eff2Val
    , regFileDisplay    = regFileElts
    , memString         = []
    , memView1Display   = memView1
    , memView2Display   = memView2
    , emuListingDisplay = procAsmListing
    , highlightedMem    = []
    , highlightedRegs   = []
    , emuAsmListing     = []
    , highlightedAsmLine = Nothing
  }

--------------------------------------------------
-- Create overall layout of the Sigma16 page
--------------------------------------------------

  helpText <- UI.div
    # set UI.class_ "HelpText"
    # set UI.id_ "DocSection"
    
-- The CentralSection contains two parts: on the left is the
-- MainSystemContainer, which has separate pages or panes for the
-- assembler, linker, emulator, etc, and on the right is the
-- HelpSection which contains the user manual.

-- It may be better to skip mainSystemSection, and to put the various
-- panes directly in mainSystemContainer.  But that seems to mean that
-- each pane has a different size, after the mainSystemContainer is
-- resized.  In either case (separate mainSystemSection in
-- mainSystemContainer, or putting the panes directly in the
-- Container) the sizes of the panes are different at the beginning,
-- but in either case after the container has been resized all the
-- panes seem to have the same size.

  mainSystemContainer <- UI.div
    # set UI.class_ "MainSystemContainer"
    # set UI.id_  "IdMainSystemContainer"
    #+ [element homePane, element editorPane,
        element asmPane,  element linkerPane,
        element procPane, element tracePane,
        element circuitPane,
--        element logPane,
        element devPane ]

  helpSection <- UI.div
    # set UI.class_ "HelpSection"
    #+ [element helpText]

-- The FullScreen, which is the top level of the html, contains the
-- TopSection (for the tabs to select different pages) and the
-- CentralSection (which holds the system GUI and the documentation).

  topSection <- UI.div
    # set UI.class_ "TopSection"
    #+ [element tabbedButtons]
  centralSection <- UI.div
    # set UI.class_ "CentralSection"
    #+ [element mainSystemContainer, element helpSection]

-- FullScreen is the top level of the html; its css definition causes
-- it to fill the browser window precisely.  If the browser window is
-- resized, the fullScreen div adjusts accordingly, so it still fills
-- the browser window.

  fullScreen <- UI.div
    # set UI.class_ "FullScreen"
    # set UI.id_ "IdFullScreen"
    #+ [element topSection, element centralSection]

  getBody window
    #+ [
-- The full GUI
         element fullScreen
-- The following are normally hidden
       , element popupMessage
       , element fileChooser
       , element modSelPopup
       , element breakpointDialogue
       ]
  setWelcomePane

--------------------------------------------------
-- Read html files and put them into gui sections
--------------------------------------------------

  let welcomeFilePath = joinPath s16dir
          </> "datafiles" </> "static"
          </> "html" </> "welcome.html"
  ys <- liftIO $ readFile welcomeFilePath
  liftIO $ putStrLn ("welcomeFilePath = " ++ welcomeFilePath)
--  liftIO $ putStrLn ys -- print contents of welcome file
  element welcomeTextArea # set UI.html ys
  let docPath = joinPath s16dir
        </> "datafiles" </> "doc"
        </> "html" </> "index.html"
  ys <- liftIO $ readFile docPath
  element helpSection # set UI.html ys

----------------------------------------
-- Finish setting up the GUI
----------------------------------------

  on UI.click topTestAbutton $ const $ do
    liftIO $ putStrLn "Top Test A button"
    element mainSystemContainer # set UI.width 200  -- does nothing

  on UI.click topTestBbutton $ const $ do
    liftIO $ putStrLn "Top Test B button"
    element mainSystemContainer # set UI.width 800

  runFunction hidePopup
  runFunction hideFileChooser
  runFunction hideModSel
  runFunction hideBreakpoint
  readjustHeights

-- initialize architecture state
  astreset <- getAct ssr actEmuState resetArchState
  s <- liftIO $ readIORef ssr
--  liftIO $ putMVar (archState s) astreset
  liftIO $ writeIORef (archState s) astreset

  doReset ssr procIOtext procInputBuffer
  setCallBufferMode Graphics.UI.Threepenny.Core.NoBuffering
  liftIO (putStrLn "setup is finished")
  return ()

----------------------------------------------------------------------
-- Accessing processor components in the GUI
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Changing to a tabbed pane
----------------------------------------------------------------------

setWelcomePane :: UI ()
setWelcomePane = do
    liftIO $ putStrLn "Welcome button clicked"
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showWelcomePane

setEditorPane :: UI ()
setEditorPane = do
    liftIO $ putStrLn "Editor button clicked"
    runFunction hideWelcomePane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showEditorPane

setAsmPane :: UI ()
setAsmPane = do
    liftIO $ putStrLn "Assembler button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showAsmPane

setLinkerPane :: UI ()
setLinkerPane = do
    liftIO $ putStrLn "Linker button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showLinkerPane

setProcPane :: UI ()
setProcPane = do
    liftIO $ putStrLn "Processor button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showProcPane

setTracePane :: UI ()
setTracePane = do
    liftIO $ putStrLn "Trace button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideCircuitPane
    runFunction hideDevPane
    runFunction showTracePane

setCircuitPane :: UI ()
setCircuitPane = do
    liftIO $ putStrLn "Circuit button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideDevPane
    runFunction showCircuitPane

setDevPane :: UI ()
setDevPane = do
    liftIO $ putStrLn "File button clicked"
    runFunction hideWelcomePane
    runFunction hideEditorPane
    runFunction hideAsmPane
    runFunction hideLinkerPane
    runFunction hideProcPane
    runFunction hideTracePane
    runFunction hideCircuitPane
    runFunction showDevPane

----------------------------------------------------------------------
-- Generate and access the register file
----------------------------------------------------------------------

mkReg :: Int -> UI (Element,Element)
mkReg i = do
  let rname = padLeftToWidth 3 ("R" ++ show i)
  labelElt <- UI.div
    # set UI.text rname
    # set UI.class_ "RFregLabel"
  valueElt <- UI.div
    # set UI.text "0000"
    # set UI.id_ rname
    # set UI.class_ "RegVal"
  return (labelElt,valueElt)

mkRegFile :: Int -> UI [(Element,Element)]
mkRegFile i
  | i>=16 = return []
  | otherwise = do
      r <- mkReg i
      rs <- mkRegFile (i+1)
      return (r:rs)

setStateRegFileDisplay
  :: SystemStateRef -> [(Element,Element)] -> UI ()
setStateRegFileDisplay ssr rfd = do
  putAct ssr actEmuState (\es -> es {regFileDisplay = rfd})

mkRegFileGrid :: [(Element,Element)] -> [[UI Element]]
mkRegFileGrid rs
  = [[element l, element v] | (l,v) <- rs]

-- Set value in the register file display

guiSetRF :: SystemStateRef -> Int -> Word16 -> UI ()
guiSetRF ssr r v = do
  liftIO $ putStrLn ("guiSetRF " ++ show r ++ " " ++ show v)
  d <- getAct ssr actEmuState regFileDisplay
  let (label,val) = d !! r
  element val # set UI.text (word16Hex v)
  return ()

guiSetMem :: SystemStateRef -> Word16 -> Word16 -> UI ()
guiSetMem ssr a v = do
  liftIO $ putStrLn ("guiSetMem " ++ word16Hex a
                     ++ " " ++ word16Hex v)
  m <- getAct ssr actEmuState memString
  let m' = setAtIndex (fromIntegral a) (updateMemString a v) m
  putAct ssr actEmuState (\s -> s {memString = m'})
  d1 <- getAct ssr actEmuState memView1Display
  d2 <- getAct ssr actEmuState memView2Display
  let xs = concat m'
  element d1 # set UI.text xs
  element d2 # set UI.text xs
  return ()

-- name should be changed to showMemLocation, but that name is
-- defined differently in Architecture ???????????

updateMemString :: Word16 -> Word16 -> String
updateMemString a v = word16Hex a ++ (' ' : word16Hex v) ++ "\n"

----------------------------------------------------------------------
-- Access the control register displays
----------------------------------------------------------------------

guiSetCtlReg
  :: UI.Widget a
  => (EmuState -> (a,a))
  -> SystemStateRef -> Word16 -> UI ()
guiSetCtlReg display ssr x = do
  (label,val) <- getAct ssr actEmuState display
  element val # set UI.text (word16Hex x)
  return ()

guiSetPc, guiSetIr, guiSetAdr, guiSetDat
  :: SystemStateRef -> Word16 -> UI ()
guiSetPc  = guiSetCtlReg pcDisplay
guiSetIr  = guiSetCtlReg irDisplay
guiSetAdr = guiSetCtlReg adrDisplay
guiSetDat = guiSetCtlReg datDisplay


----------------------------------------------------------------------
-- Execute instructions
----------------------------------------------------------------------

-- Run an instruction in the UI monad.  The emulation is carried out
-- by runStep in the StateT EmuState IO monad, which records effects
-- and state changes in ArchState.  The changes to the user interface
-- are then carried out by doStep, back in the UI monad.

checkDoStep :: SystemStateRef -> UI ()
checkDoStep ssr = do
  status <- liftIO $ getProcStatus ssr
--  liftIO $ putStrLn ("checkDoStep: procStatus = " ++ show status)
  case status of
    Reset      -> return ()
    Ready      -> doStep ssr >> return ()
    Running    -> doStep ssr >> return ()
    Halted     -> return ()
    Paused     -> doStep ssr >> return ()
    Blocked    -> do
      liftIO $ setProcStatus ssr Ready
      doStep ssr
      return ()
    Breakpoint -> doStep ssr >> return ()

doStep :: SystemStateRef -> UI ArchState
doStep ssr = do
  s <- liftIO $ readIORef ssr
--  ast1 <- liftIO $ takeMVar (archState s)
  ast1 <- liftIO $ readIORef (archState s)
  let addrExecutedInstruction = astPC ast1
--  liftIO $ putStrLn ("doStep executing at pc=" ++
--                      show addrExecutedInstruction)
  (r,ast2) <- runStateT     -- Nothing:  no breakpoint
    (executeIfRunning ssr recordEffect recordInstruction)
    ast1
  status <- liftIO $ getProcStatus ssr
--  liftIO $ putStrLn ("doStep: r,status=" ++ show (r,status))
--  liftIO $ putMVar (archState s) ast2
  liftIO $ writeIORef (archState s) ast2
  return ast2
-- optionally can do processInstructionEffects ssr ast2


-- Multi step is similar to run, but it does step repeatedly, in
-- order to get the display updated after each instruction

data StepMode
  = StepFullDisplay
  | StepFast
  | StepSlow Int
  deriving Show

-- This is the main interface used by commands to run a sequence of
-- instructions

runMultiStep :: SystemStateRef -> IORef Bool -> StepMode -> UI ()
runMultiStep ssr pauseIORef mode = do
-- Define run: fork a cocurrent thread to run instructions repeatedly
-- until a stopping condition occurs
  liftIO $ writeIORef pauseIORef False
  let run = do
        liftIO $ setProcStatus ssr Running
        setInstrCountBusy ssr
        s <- liftIO $ readIORef ssr
        ast <- liftIO $ readIORef (archState s)
        refreshProcStatus ssr ast
        flushCallBuffer
        w <- askWindow
        liftIO $ forkRunMultiStep ssr pauseIORef mode w
        return ()
  let doNothing = return ()
-- Check whether processor is in a valid state to start running;
-- if not just do nothing
  status <- liftIO $ getProcStatus ssr
  case status of
         Reset      -> doNothing
         Ready      -> run
         Running    -> doNothing
         Halted     -> doNothing
         Paused     -> run
         Blocked    -> run
         Breakpoint -> run

forkRunMultiStep
  :: SystemStateRef
  -> IORef Bool
  -> StepMode
  -> Window
  -> IO ()
forkRunMultiStep ssr pauseIORef mode w = do
  forkIO $ runUI w $ checkDoMultiStep ssr pauseIORef mode
  return ()

-- Set the instruction count field to ... while processor is running
setInstrCountBusy :: SystemStateRef -> UI ()
setInstrCountBusy ssr = do
  iCountElt <- getAct ssr actEmuState instrCountDisplay
  element iCountElt # set UI.text "..."
  flushCallBuffer

refreshInstrCount :: SystemStateRef -> ArchState -> UI ()
refreshInstrCount ssr ast = do
  iCountElt <- getAct ssr actEmuState instrCountDisplay
  element iCountElt # set UI.text (show (astInstrCount ast))
  flushCallBuffer
  return ()

checkDoMultiStep :: SystemStateRef -> IORef Bool -> StepMode -> UI ()
checkDoMultiStep ssr pauseIORef mode = do
  status <- liftIO $ getProcStatus ssr
--  liftIO $ putStrLn ("checkDoStep: procStatus = " ++ show status)
  let finish = do
        s <- liftIO $ readIORef ssr
        ast <- liftIO $ readIORef (archState s)
        clearEffects ssr
        refreshDisplay ssr  -- after run, do complete refresh
        flushCallBuffer
        return ()
  case status of
    Reset      -> finish -- return ()
    Ready      -> doMultiStep ssr pauseIORef mode
    Running    -> doMultiStep ssr pauseIORef mode
    Halted     -> finish -- return ()
    Paused     -> finish -- return ()
    Blocked    -> do
      liftIO $ setProcStatus ssr Running
      doMultiStep ssr pauseIORef mode
    Breakpoint -> finish -- return ()

doMultiStep :: SystemStateRef -> IORef Bool -> StepMode -> UI ()
doMultiStep ssr pauseIORef mode = do
  ast <- doStep ssr
  status <- liftIO $ getProcStatus ssr
  mbp <- getAct ssr actUiState uiBreakpoint
  let break = case mbp of
        Nothing -> False
        Just bexp -> evalBPbool bexp ast
  pause <- liftIO $ readIORef pauseIORef
  let status' = newStatus status break pause
--  liftIO $ putStrLn ("(status, status', break, pause) = "
--             ++ show (status, status', break, pause))
  liftIO $ setProcStatus ssr status'
  case mode of
    StepFullDisplay ->
      processInstructionEffects ssr ast
    StepSlow i -> do
      processInstructionEffects ssr ast
      liftIO $ threadDelay i
    StepFast -> do
      clearEffects ssr
      return ()
  flushCallBuffer
  checkDoMultiStep ssr pauseIORef mode

-- Run until halt, break, pause.  If blocked, resume execution; this
-- should allow the user interface to update the I/O buffers.

checkDoRun :: SystemStateRef -> IORef Bool -> UI ()
checkDoRun ssr pauseIORef = do
  status <- liftIO $ getProcStatus ssr
  w <- askWindow
  let f = do
        liftIO $ runWrapper ssr pauseIORef w
        status' <- liftIO $ getProcStatus ssr
        case status' of
          Reset      -> return ()
          Ready      -> f
          Running    -> f
          Halted     -> return ()
          Paused     -> return ()
          Blocked    -> do
            liftIO $ setProcStatus ssr Running
            f
          Breakpoint -> return ()
  case status of
    Reset      -> return ()
    Ready      -> f
    Running    -> f
    Halted     -> return ()
    Paused     -> return ()
    Blocked    -> do
      liftIO $ setProcStatus ssr Running
      f
    Breakpoint -> return ()

runWrapper :: SystemStateRef -> IORef Bool -> Window -> IO ()
runWrapper ssr pauseIORef w = do
  forkIO $ runUI w $ doRun ssr pauseIORef
  return ()

doRun :: SystemStateRef -> IORef Bool -> UI ()
doRun ssr pauseIORef = do
  mbp <- getAct ssr actUiState uiBreakpoint
  s <- liftIO $ readIORef ssr
--  ast1 <- liftIO $ takeMVar (archState s)
  liftIO $ setProcStatus ssr Running
  ast1 <- liftIO $ readIORef (archState s)
  liftIO $ writeIORef pauseIORef False
  (r,ast2) <- runStateT
              (run recordEffect recordInstruction mbp pauseIORef)
              ast1
--  liftIO $ putMVar (archState s) ast2
  liftIO $ writeIORef (archState s) ast2
--  liftIO $ putStrLn ("run...... status = " ++
--                       show (astProcStatus ast2))
  let effs = astEffects ast2
  handleEffectsNew ssr effs
  displayDecodedInstruction ssr
  processInstructionEffects ssr ast2
--  liftIO $ putStrLn ("doRun returning")
  return ()

----------------------------------------------------------------------
-- Displaying results of instruction execution
----------------------------------------------------------------------

-- processInstructionEffects.  After doStep, update the display to
-- show the results of the last instruction executed.  This is used
-- after doStep, and also after doRun to show the results of the last
-- instruction in a sequence of instructions that have executed.

processInstructionEffects
  :: SystemStateRef
  -> ArchState
  -> UI ()
processInstructionEffects ssr ast = do
  let addrExInstr = astAddrExInstr ast
  procSt <- liftIO $ getProcStatus ssr
  e <- getAct ssr actEmuState procStatusDisplay
  element e # set UI.html (show procSt)
--  liftIO $ putStrLn ("processInstructionEffects")
--  liftIO $ putStrLn ("procStatus=" ++ show procSt)
  let effs = astEffects ast
--  liftIO $ putStrLn ("doStep effects = " ++ show effs)

-- Display the instruction that has executed
  displayDecodedInstruction ssr

-- Handle effects on the regfile and memory done in handleEffectsNew

-- commented out and moved to handleEffectsNew, need these defs:
  let newRegEffects = filter isRegEffect effs
  let newMemEffects = filter isMemEffect effs

-- Highlight current assembly line
  mCurModule <- getCurrentModule ssr
  case mCurModule of
    Nothing -> return ()
    Just m -> do
      let lat = getLat m
--        let activeLine = 1 + lookupLineAddr (astPC ast2) lat
      let activeLine =
            lookupLineAddr addrExInstr lat
              + 2  -- account for header lines
--      liftIO $ putStrLn ("highlight asm lat = " ++ show lat)
--      liftIO $ putStrLn ("addrExecutedInstruction = " ++
--                 show addrExInstr)
--        liftIO $ putStrLn ("astPC = " ++ show (astPC ast))
--      liftIO $ putStrLn ("activeline = " ++ show activeLine)
      listing <- getAct ssr actEmuState emuAsmListing
      oldLine <- getAct ssr actEmuState highlightedAsmLine
      handleListingLine ssr listing oldLine activeLine
      return ()

  handleEffectsNew ssr effs
  clearEffects ssr
--  liftIO $ putStrLn "Processor Step finished"
  return ()

----------------------------------------------------------------------
-- Processor display
----------------------------------------------------------------------

-- handleListingLine: highlight the current assembly source line

handleListingLine
  :: SystemStateRef     -- ssr
  -> [String]           -- listing: assembly listing
  -> Maybe (Int,String) -- oldLine previously highlighted
  -> Int                -- index of current line to be highlighted
  -> UI ()
handleListingLine ssr listing oldLine k = do
--  liftIO $ putStrLn ("handleListingLine oldLine=" ++ show oldLine
--              ++ "  k=" ++ show k)
  let xs = case oldLine of
             Nothing -> listing
             Just (i,ys) -> setAtIndex i ys listing
--  let ys = setAtIndex k (markModified (xs !! k)) xs
  let ys = setAtIndex k (markAsmCurrentLine (xs !! k)) xs
  e <- getAct ssr actEmuState emuListingDisplay
  element e # set UI.html (concat ys)
  pxPerLine <- getPxPerLine ssr  -- convert lines to pixels
--  callFunction (scrollFromTop "ProcAsmListing" (pxPerLine * k))
--  callFunction (scrollAsmLine "ProcAsmListing" (pxPerLine * k))
  runFunction scrollAsmLine
  return ()
  
-- Helper functions for displaying an instruction step


-- Code for handling register effects

regEffectPriority :: EmuEffect -> EmuEffect -> EmuEffect
regEffectPriority x@(GetRF _ _) y@(GetRF _ _) = x
regEffectPriority x@(SetRF _ _ _) y@(SetRF _ _ _) = x
regEffectPriority x@(GetRF _ _) y@(SetRF _ _ _) = y
regEffectPriority x@(SetRF _ _ _) y@(GetRF _ _) = x
regEffectPriority x@_ y@(RegHighlighted _ _) = x
regEffectPriority x@(RegHighlighted _ _) y@_ = y
regEffectPriority x y = x

-- Make sure that two effects on the same register are handled right
checkRegNums :: [EmuEffect] -> [EmuEffect]
checkRegNums [] = []
checkRegNums [x] = [x]
checkRegNums (x:y:ys) =
  if effectRegNum x == effectRegNum y
    then checkRegNums (regEffectPriority x y : ys)
    else x : checkRegNums (y:ys)

handleRegEffects :: SystemStateRef -> [EmuEffect] -> UI ()
handleRegEffects ssr []  = return ()
handleRegEffects ssr (e:effs) = do
  updateRegDisplay ssr e
  handleRegEffects ssr effs

updateRegDisplay :: SystemStateRef -> EmuEffect -> UI ()
updateRegDisplay ssr eff = do
  rfd <- getAct ssr actEmuState regFileDisplay
--  liftIO $ putStrLn ("updateRegDisplay " ++ show eff)
  case eff of
    GetRF r v -> do
      let (lblDisp,valDisp) = rfd !! (fromIntegral r)
      element valDisp # set UI.html
        (markFetched (word16Hex v))
      return ()
    SetRF r old new -> do
      let (lblDisp,valDisp) = rfd !! (fromIntegral r)
      element valDisp # set UI.html
        (markModified (word16Hex new))
      return ()
    RegHighlighted r v -> do
      let (lblDisp,valDisp) = rfd !! (fromIntegral r)
      element valDisp # set UI.html
        (word16Hex v)
      return ()
    _ -> return ()

showNewRegLoc :: Integral a => a -> EmuEffect -> String
showNewRegLoc _ (GetRF a v) = markFetched (displayRegString a v)
showNewRegLoc _ (SetRF a old new) =
  markModified (displayRegString a new)
showNewRegLoc _ (RegHighlighted a v) = displayRegString a v ++ "\n"

-- Change a register or memory effect to RegHighlighted or
-- MemHighlighted.  After these have been saved in the old effects
-- state, doStep will be able to un-highlight the old effects and to
-- highlight the new ones.

noteHighlighted :: EmuEffect -> EmuEffect
noteHighlighted (GetRF a v) = RegHighlighted a v
noteHighlighted (SetRF a old new) = RegHighlighted a new
noteHighlighted (GetMem a v) = MemHighlighted a v
noteHighlighted (SetMem a old new) = MemHighlighted a new
noteHighlighted _ = NoEffect

-- Predicate for filtering effects to find regory effects
isRegEffect :: EmuEffect -> Bool
isRegEffect (GetRF _ _) = True
isRegEffect (SetRF _ _ _) = True
isRegEffect (RegHighlighted _ _) = True
isRegEffect _ = False

effectRegNum :: EmuEffect -> RegAddress
effectRegNum (GetRF a _) = a
effectRegNum (SetRF a _ _) = a
effectRegNum (RegHighlighted a v) = a
effectRegNum _ = 0

displayRegString :: Word8 -> Word16 -> String
displayRegString a v =
  'R' : (show (fromIntegral a) ++ (' ' : word16Hex v))

-- Code for handling memory effects

-- There may be more than one memory effect on the same address.  When
-- this happens, the right one needs to be used to update the display.
-- Stores are highlighted in preference to fetches, and highlights
-- from the previous instruction are ignored.  This function chooses
-- which effect to use; a precondition is that both effect arguments
-- refer to the same address.

memEffectPriority :: EmuEffect -> EmuEffect -> EmuEffect
memEffectPriority x@(GetMem _ _) y@(GetMem _ _) = x
memEffectPriority x@(SetMem _ _ _) y@(SetMem _ _ _) = x
memEffectPriority x@(GetMem _ _) y@(SetMem _ _ _) = y
memEffectPriority x@(SetMem _ _ _) y@(GetMem _ _) = x
memEffectPriority x@_ y@(MemHighlighted _ _) = x
memEffectPriority x@(MemHighlighted _ _) y@_ = y
memEffectPriority x y = x

checkAddresses :: [EmuEffect] -> [EmuEffect]
checkAddresses [] = []
checkAddresses [x] = [x]
checkAddresses (x:y:ys) =
  if effectAddress x == effectAddress y
    then memEffectPriority x y : checkAddresses (y:ys)
    else x : checkAddresses (y:ys)

handleMemEffects :: Integral a
  => [(a,String)] -> [EmuEffect] -> [String]
handleMemEffects [] _ = []
handleMemEffects xs [] = Data.List.map snd xs
handleMemEffects ((a,m):xs) (e:effs) =
  if fromIntegral a == fromIntegral (effectAddress e)
    then showNewMemLoc a e : handleMemEffects xs effs
    else m : handleMemEffects xs (e:effs)

showNewMemLoc :: Integral a => a -> EmuEffect -> String
showNewMemLoc _ (GetMem a v) = markFetched (updateMemString a v)
showNewMemLoc _ (SetMem a old new) =
  markModified (updateMemString a new)
showNewMemLoc _ (MemHighlighted a v) = updateMemString a v ++ "\n"

-- Change a Get/Set mem to MemHighlighted.  This enables doStep to
-- know whether there is a new effect to handle, or just an old one to
-- undo.

memEffectToHighlight :: EmuEffect -> EmuEffect
memEffectToHighlight (GetMem a v) = MemHighlighted a v
memEffectToHighlight (SetMem a old new) = MemHighlighted a new
memEffectToHighlight _ = NoEffect

-- Predicate for filtering effects to find memory effects
isMemEffect :: EmuEffect -> Bool
isMemEffect (GetMem _ _) = True
isMemEffect (SetMem _ _ _) = True
isMemEffect (MemHighlighted _ _) = True
isMemEffect _ = False

effectAddress :: EmuEffect -> Word16
effectAddress (GetMem a _) = a
effectAddress (SetMem a _ _) = a
effectAddress (MemHighlighted a v) = a
effectAddress _ = 0

-- Get first and second instruction effect, if any
getEff1, getEff2 :: [EmuEffect] -> String
getEff1 (x:_) = showMainEffect x
getEff1 _ = ""
getEff2 (x:y:_) = showMainEffect y
getEff2 _ = ""


--------------------------------------------------
-- Handle effects
--------------------------------------------------

handleEffectsNew :: SystemStateRef -> [EmuEffect] -> UI ()
handleEffectsNew ssr effs = do
--  liftIO $ putStrLn ("handleEffects " ++ show effs)
  s <- liftIO $ readIORef ssr
--  ast <- liftIO $ takeMVar (archState s)
  ast <- liftIO $ readIORef (archState s)
--  liftIO $ putMVar (archState s) ast
  
--  ast <- getAct ssr actEmuState archState

-- Instruction count  

{-
  let icount = astInstrCount ast
  iCountDisplay <- getAct ssr actEmuState instrCountDisplay
  element iCountDisplay # set UI.text (show icount)
-}

-- Instruction control registers

  let xs = case foldl' findSetPc Nothing effs of
             Nothing -> word16Hex (astPC ast)
             Just (SetPC old new) -> showModifiedVal new
  (lbl,val) <- getAct ssr actEmuState pcDisplay
  element val # set UI.html xs
  let xs = case foldl' findSetIr Nothing effs of
        Nothing -> word16Hex (astIR ast)
        Just (SetIR old new) -> showModifiedVal new
  (lbl,val) <- getAct ssr actEmuState irDisplay
  element val # set UI.html xs
  let xs = case foldl' findSetAdr Nothing effs of
        Nothing -> word16Hex (astAD ast)
        Just (SetAD old new) -> showModifiedVal new
  (lbl,val) <- getAct ssr actEmuState adrDisplay
  element val # set UI.html xs
  let xs = case foldl' findSetDat Nothing effs of
        Nothing -> word16Hex (astDAT ast)
        Just (SetDAT old new) -> showModifiedVal new
  (lbl,val) <- getAct ssr actEmuState datDisplay
  element val # set UI.html xs

-- Effects on the register file

--  liftIO $ putStrLn "*** Handle regfile effects *** "
  oldRegEffects <- getAct ssr actEmuState highlightedRegs
  let newRegEffects = filter isRegEffect effs
  let allRegEffects =
        sortOn effectRegNum (oldRegEffects ++ newRegEffects)
  let checkedRegEffects = checkRegNums allRegEffects
--  liftIO $ putStrLn ("old reg effects = " ++ show oldRegEffects)
--  liftIO $ putStrLn ("new reg effects = " ++ show newRegEffects)
--  liftIO $ putStrLn ("all reg effects = " ++ show allRegEffects)
--  liftIO $ putStrLn ("checked reg effects = " ++ show checkedRegEffects)
  handleRegEffects ssr checkedRegEffects

-- Effects on the memory

--  liftIO $ putStrLn "*** Handle memory effects *** "
  ms <- getAct ssr actEmuState memString
  -- ms is a list of strings, one for each memory location
  oldMemEffects <- getAct ssr actEmuState highlightedMem
  let newMemEffects = filter isMemEffect effs
  let allMemEffects =
        checkAddresses 
          (sortOn effectAddress (oldMemEffects ++ newMemEffects))
--  liftIO $ putStrLn ("old mem effects = " ++ show oldMemEffects)
--  liftIO $ putStrLn ("new mem effects = " ++ show newMemEffects)
--  liftIO $ putStrLn ("all mem effects = " ++ show allMemEffects)
  let ms' = handleMemEffects (zip [0..] ms) allMemEffects
  putAct ssr actEmuState (\s -> s {memString = ms'})
  let xs = concat ms'
  mv1 <- getAct ssr actEmuState memView1Display
  mv2 <- getAct ssr actEmuState memView2Display
  element mv1 # set UI.html xs
  element mv2 # set UI.html xs

-- Save the new effects so they can be cleared on the next step

  putAct ssr actEmuState
      (\s -> s {
                 highlightedMem =
                   Data.List.map noteHighlighted newMemEffects
               , highlightedRegs =
                   Data.List.map noteHighlighted newRegEffects
               })

  return ()

-- Remove any effects stored in the archState, so they won't be there
-- when new effects from the next instruction are added

clearEffects :: SystemStateRef -> UI ()
clearEffects ssr = do
  s <- liftIO $ readIORef ssr
  ast <- liftIO $ readIORef (archState s)
  liftIO $ writeIORef (archState s) (ast {astEffects = []})
  return ()

-- The code for handling registers is similar to that for memory.
-- Currently it's just replicated and modified; it would be good to
-- clean it up and exploit the commonality between the reg and mem
-- displays.  The memory is held in the EmuState as a list of strings,
-- with one element for each memory location.  When it's to be
-- displayed, this list is concatenated.  In contrast, the register
-- file consists of distinct elements, and only the ones that need to
-- be changed are affected.  The state of the register file display is
-- not saved in the EmuState.


-- Display the instruction that has executed

displayDecodedInstruction :: SystemStateRef -> UI ()
displayDecodedInstruction ssr = do
  s <- liftIO $ readIORef ssr
--  ast <- liftIO $ takeMVar (archState s)
  ast <- liftIO $ readIORef (archState s)
--  liftIO $ putMVar (archState s) ast
--  ast <- getAct ssr actEmuState archState
--  liftIO $ putStrLn ("Step button inst = " ++
--                        show (astInstruction ast))
  let (Instruction decoded symbol mainEffects) = astInstruction ast
  e <- getAct ssr actEmuState opDisplay
  element e # set UI.text (show symbol)
  e <- getAct ssr actEmuState argsDisplay
  element e # set UI.text (showInstructionArgs decoded)    
  e <- getAct ssr actEmuState ccDisplay
  let cc = fetchRF (astRF ast) 15
  element e # set UI.text (showCCbits cc)
  e <- getAct ssr actEmuState fmtDisplay
  element e # set UI.text (show (instructionFormat decoded))
  e <- getAct ssr actEmuState eff1Display
  element e # set UI.text (getEff1 mainEffects)
  e <- getAct ssr actEmuState eff2Display
  element e # set UI.text (getEff2 mainEffects)
  return ()


-- Show a word in hex, with html tags to indicate it is modified and
-- should be highlighted

findSetPc :: Maybe EmuEffect -> EmuEffect -> Maybe EmuEffect
findSetPc (Just x) _ = Just x
findSetPc Nothing (SetPC old new) = Just (SetPC old new)
findSetPc Nothing x = Nothing

findSetIr :: Maybe EmuEffect -> EmuEffect -> Maybe EmuEffect
findSetIr (Just x) _ = Just x
findSetIr Nothing (SetIR old new) = Just (SetIR old new)
findSetIr Nothing x = Nothing

findSetAdr :: Maybe EmuEffect -> EmuEffect -> Maybe EmuEffect
findSetAdr (Just x) _ = Just x
findSetAdr Nothing (SetAD old new) = Just (SetAD old new)
findSetAdr Nothing x = Nothing

findSetDat :: Maybe EmuEffect -> EmuEffect -> Maybe EmuEffect
findSetDat (Just x) _ = Just x
findSetDat Nothing (SetDAT old new) = Just (SetDAT old new)
findSetDat Nothing x = Nothing

markFetched, markModified, markAsmCurrentLine :: String -> String
-- asm listing better without the newline, mem better with it...
markModified x = "<span class=\"MO\">" ++ x ++ "</span>"
markFetched x =  "<span class=\"FE\">" ++ x ++ "</span>"
markAsmCurrentLine x =  "<span class = \"MO\" id=\"ASMCU\">" ++ x ++ "</span>"

showFetchedVal, showModifiedVal :: Word16 -> String
showFetchedVal x = markFetched (word16Hex x)
showModifiedVal x = markModified (word16Hex x)

refreshProcStatus :: SystemStateRef -> ArchState -> UI ()
refreshProcStatus ssr ast = do
  s <- liftIO $ getProcStatus ssr
  e <- getAct ssr actEmuState procStatusDisplay
  liftIO $ putStrLn ("refreshProcStatus: " ++ show s)
  element e # set UI.html (show s)
  return ()

-- refreshDisplay: obtain the values of registers, memory and the
-- emulator state and use them to refresh the corresponding elements
-- in the GUI.

refreshDisplay :: SystemStateRef -> UI ()
refreshDisplay ssr = do
  liftIO $ putStrLn "refreshDisplay"
  s <- liftIO $ readIORef ssr
--  ast <- liftIO $ takeMVar (archState s)
  ast <- liftIO $ readIORef (archState s)
--  liftIO $ putMVar (archState s) ast
  refreshProcStatus ssr ast
--  as <- getAct ssr actEmuState archState
-- Refresh emulator state  
  e <- getAct ssr actEmuState opDisplay
  element e # set UI.text ""
  e <- getAct ssr actEmuState argsDisplay
  element e # set UI.text ""
  e <- getAct ssr actEmuState fmtDisplay
  element e # set UI.text ""
  e <- getAct ssr actEmuState eff1Display
  element e # set UI.text ""
  e <- getAct ssr actEmuState eff2Display
  element e # set UI.text ""
  iCountElt <- getAct ssr actEmuState instrCountDisplay
  element iCountElt # set UI.text (show (astInstrCount ast))
-- Refresh processor control registers  
  (pcLbl,pcVal) <- getAct ssr actEmuState pcDisplay
  element pcVal # set UI.text (word16Hex (astPC ast))
  (irLbl,irVal) <- getAct ssr actEmuState irDisplay
  element irVal # set UI.text (word16Hex (astIR ast))
  (adrLbl,adrVal) <- getAct ssr actEmuState adrDisplay
  element adrVal # set UI.text (word16Hex (astAD ast))
-- Refresh register file
  refreshRegFile ssr
  hwm <- getAct ssr actEmuState memHighWaterMark
  liftIO $ putStrLn ("refresh display: hwm = " ++ show hwm)
-- Refresh memory  
  let mem = astMem ast
  let xs = [showMemLocation mem a | a <- [0..hwm]]
  putAct ssr actEmuState (\s -> s {memString = xs})
  let ys = concat xs
  mv1 <- getAct ssr actEmuState memView1Display
  mv2 <- getAct ssr actEmuState memView2Display
  element mv1 # set UI.html ys
  element mv2 # set UI.html ys
  return ()


refreshRegFile :: SystemStateRef -> UI ()
refreshRegFile ssr = do
  s <- liftIO $ readIORef ssr
--  ast <- liftIO $ takeMVar (archState s)
  ast <- liftIO $ readIORef (archState s)
  let rf = astRF ast :: RegFile
--  liftIO $ putMVar (archState s) ast
  rfDisplay <- getAct ssr actEmuState regFileDisplay
  let looper i
        | i>15  = return ()
        | otherwise = do
            let (labelElt,valElt) = rfDisplay !! (fromIntegral i)
            element valElt # set UI.text
              (word16Hex (fetchRF rf (fromIntegral i)))
            looper (i+1)
  looper 0

-- doReset: Put the processor and the emulator into the initial state
-- and refresh the display

doReset :: SystemStateRef -> Element -> Element -> UI ()
doReset ssr procIOtext procInputBuffer = do
  liftIO $ putStrLn "Reset the processor"
  e <- getAct ssr actEmuState emuListingDisplay
  element e # set UI.html "" -- Clear assembly listing
  s <- liftIO $ readIORef ssr
--  ast <- liftIO $ takeMVar (archState s)  -- will discard old state
  ast <- liftIO $ readIORef (archState s)  -- will discard old state
  ast' <- getAct ssr actEmuState resetArchState
--  liftIO $ putMVar (archState s) ast'
  liftIO $ writeIORef (archState s) ast'
--  element procIOtext # set UI.html "<html><br></html>"
--  element procIOtext # set UI.html ""
  element procIOtext # set UI.value ""
--  element procInputBuffer # set UI.html ""
  element procInputBuffer # set UI.value ""
--  putAct ssr actEmuState (\es -> es {procStatus = Reset})
  liftIO $ setProcStatus ssr Reset
  refreshDisplay ssr
  return ()
