module State where

-- Mutable state with concurrency and mutual exclusion

import Control.Concurrent
import Control.Exception
import Control.Monad.State
-- import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements
import Data.IORef
import Data.Word

import Arithmetic
import Architecture
import Common

-- This module supports

--   *  concurrent threads (to keep the system responsive)
--   *  mutual exclusion (to maintain safe access to state)
--   *  state contains separate data for each activity
--   *  each activity is protected by an MVar holding its state
--   *  enforce mutual exlusion within an activity, allow concurrency
--        across different activities
--   *  exceptions
--   *  state transformer monad for running actions; the action
--        may want to update the activity state many times, not just
--        at the end

-- Each GUI action has type IO (). With the Threepenny library, an
-- action has type UI a, and liftIO is used.  To keep the GUI
-- responsive, an action is performed in a thread created by forkIO.
-- Long running actions (such as running a program on the emulator)
-- may use State monad internally.  Each action is protected by a
-- catch, so if it fails it does so gracefully.  The full GUI state is
-- an MVar, which is a record with subfields.  Each subfield of the
-- GUI state is controlled by an MVar; thus only one action at a time
-- can be using a particular subfield.  Even if a subfield is locked,
-- other actions can still proceed using other subfields.

-- An activity is a set of related actions with their associated
-- state.  Each activity is represented as an MVar holding its state.
-- The MVar provides mutual exclusion; only one action at a time can
-- be in progress for an activity, but several different activities
-- may have actions running concurrently.

----------------------------------------
-- System state and activities
----------------------------------------

----------------------------------------
-- Running an action
----------------------------------------

-- The global state is a record of MVars.  An action may be performed
-- using the value of any of these MVars.  While the action is
-- running, its MVar is locked so it has exclusive access to this
-- state.  The action itself runs in the StateT monad, so it can
-- perform a sequence of private state transformations.

-- a is global state, a record of MVars
-- b is type of activity data, in one of the MVars in global state
-- c is return type of the executed action

executeAction
  :: IORef a             -- ssr = system state reference
  -> (a -> MVar b)       -- f = projection to get activity
  -> StateT b IO c       -- g = action to be executed
  -> (IOError -> IO ())  -- h = exception handler
  -> IO (Maybe c)        -- put back updated state, return result

executeAction ssr f g h =
  do s <- readIORef ssr
     let amv = f s                -- get activity from global state
     mstate1 <- tryTakeMVar amv   -- try to get exclusive access
     case mstate1 of
       Nothing -> return Nothing  -- busy, give up, no result
       Just state1 -> do          -- have exclusive access, do action
         mutex <- newEmptyMVar    -- hold result, wait for termination
         tid <- forkIO $ do       -- run action in its own thread
           y <- catch
             (do x <- runStateT g state1
                 putMVar mutex (Just x)
                 return ())
             (\e -> do
                 h e
                 putMVar mutex Nothing
                 return ())
           return ()
         z <- takeMVar mutex      -- wait for termination
         case z of
           Nothing -> do
             putMVar amv state1   -- failed, put back original state
             return Nothing       -- return no result
           Just (result,state2) -> do
             putMVar amv state2   -- put back updated state
             return (Just result) -- succeeded

----------------------------------------
-- Handling IO errors
----------------------------------------

ignoreIOerr :: IOError -> IO ()
ignoreIOerr e =
  putStrLn "Ignoring an IOerror condition"

----------------------------------------
-- Quick access to an activity state
----------------------------------------

-- getAct and putAct are for getting part of the state of an activity,
-- or modifying it, but not for performing calculations on the state
-- that might take a significant amount of time.  These operations
-- should be fast, and are unsuitable for long computations that could
-- reduce responsiveness of the program.

-- getAct will just take the MVar for the activity; if it gets it, it
-- will return quickly and release the mutex, but if it can't get it
-- it will block until the other operation on this same activity
-- finishes.  Operations that might take a long time should be in a
-- separate activity than just data fields that are accessed
-- quickly.

getAct
  :: IORef a
  -> (a -> MVar b)
  -> (b->c)
  -> UI c

getAct ssr getx f =
  do s <- liftIO $ readIORef ssr
     let amv = getx s
     ast <- liftIO $ takeMVar amv
     let x = f ast
     liftIO $ putMVar amv ast
     return x

putAct
  :: IORef a
  -> (a -> MVar b)
  -> (b->b)
  -> UI ()

putAct ssr getx f =
  do s <- liftIO $ readIORef ssr
     let amv = getx s
     ast <- liftIO $ takeMVar amv
     let ast' = f ast
     liftIO $ putMVar amv ast'
     return ()

----------------------------------------
-- Mutable system state and concurrent activities
----------------------------------------

-- All state, including the GUI, the emulation, access to files, etc.,
-- is held in the SystemState.  A pointer to this is passed around as
-- a value (typically with the name ssr) of type SystemStateRef; there
-- are no mutable global variables.

type SystemStateRef = IORef SystemState

-- The current value of the system state is defined as a record with
-- separate fields corresponding to distinct activities; each activity
-- has its own state which is held in an MVar.  Work on one activity
-- is done in a critical region (e.g. only one thread at a time can
-- be operating on the processor state) but work in different
-- activities can proceed concurrently (e.g. the user interface
-- remains responsive while a program is being emulated).

data SystemState = SystemState
  { actUiState   :: MVar UiState     -- state of user interface
  , actEmuState  :: MVar EmuState    -- state of emulator
  , archState    :: IORef ArchState
  }

-- The initial system state is built by creating empty MVars for the
-- components.  After the gui is constructed, the MVars are set to
-- thei initial values.

initialSystemState :: UI (IORef SystemState)
initialSystemState = do
  mvUiState <- liftIO newEmptyMVar
  mvEmuState <- liftIO newEmptyMVar
--  mvArchState <- liftIO newEmptyMVar
  iorefArchState <- liftIO $ newIORef initArchState
  let s = SystemState
            { actUiState = mvUiState
            , actEmuState = mvEmuState
            , archState = iorefArchState
            }
  ssr <- liftIO $ newIORef s
  return ssr


-- get/set proc status in the StateT ArchState UI monad

astgetProcStatus :: StateT ArchState UI ProcStatus
astgetProcStatus = do
  s <- Control.Monad.State.get
  return (astProcStatus s)

astsetProcStatus :: ProcStatus -> StateT ArchState UI ()
astsetProcStatus status = do
  s <- Control.Monad.State.get
  Control.Monad.State.put (s {astProcStatus = status})

-- get/set proc status in the IO monad

getProcStatus :: SystemStateRef -> IO ProcStatus
getProcStatus ssr = do
  s <- readIORef ssr
  ast <- readIORef (archState s)
  return (astProcStatus ast)

setProcStatus :: SystemStateRef -> ProcStatus -> IO ()
setProcStatus ssr status = do
  s <- readIORef ssr
  ast <- readIORef (archState s)
  writeIORef (archState s) (ast {astProcStatus = status})
  return ()

----------------------------------------------------------------------
-- User interface state
----------------------------------------------------------------------

data UiState = UiState
  { currentPath          :: Maybe FilePath
  , edProvisionalPath    :: Maybe FilePath  -- file chooser temp state
  , edText              :: Maybe String    -- text editor
  , edProvisionalText   :: Maybe String
  , edProvisionalDirContents :: Maybe [FilePath]
  , edTextLastSaved     :: Maybe String    -- last version in file
  , s16program          :: S16program
  , s16CurrentMod       :: Int             -- index into program
  , uiBreakpoint        :: Maybe BPbool
  , textInput           :: Maybe String    -- for testing
  , pxPerLine           :: Int          -- pixels per line of text
  }

-- The editor buttons, OpenFile and SaveAs, set the fileChooserAction
-- in the system state.  The file chooser then looks 
-- data fileChooserAction = fcOpen | fcSaveAs
--  deriving Show

getCurrentPath :: SystemStateRef -> UI (Maybe FilePath)
getCurrentPath ssr =
  do x <- getAct ssr actUiState currentPath
     return x
putCurrentPath :: SystemStateRef -> Maybe FilePath -> UI ()
putCurrentPath ssr x =
  putAct ssr actUiState (\a -> a {currentPath = x})

{- deprecated
, currentFileName     :: Maybe FilePath
getCurrentFileName :: SystemStateRef -> UI (Maybe FilePath)
getCurrentFileName ssr =
  do x <- getAct ssr actUiState currentFileName
     return x
putCurrentFileName :: SystemStateRef -> Maybe FilePath -> UI ()
putCurrentFileName ssr x =
  putAct ssr actUiState (\a -> a {currentFileName = x})
-}

getEdProvisionalPath :: SystemStateRef -> UI (Maybe FilePath)
getEdProvisionalPath ssr =
  do x <- getAct ssr actUiState edProvisionalPath
     return x
putEdProvisionalPath :: SystemStateRef -> Maybe FilePath -> UI ()
putEdProvisionalPath ssr x =
  putAct ssr actUiState (\a -> a {edProvisionalPath = x})


getEdText :: SystemStateRef -> UI (Maybe String)
getEdText ssr =
  do x <- getAct ssr actUiState edText
     return x
putEdText :: SystemStateRef -> Maybe String -> UI ()
putEdText ssr x =
  putAct ssr actUiState (\a -> a {edText = x})

getEdProvisionalText :: SystemStateRef -> UI (Maybe String)
getEdProvisionalText ssr =
  do x <- getAct ssr actUiState edProvisionalText
     return x
putEdProvisionalText :: SystemStateRef -> Maybe String -> UI ()
putEdProvisionalText ssr x =
  putAct ssr actUiState (\a -> a {edProvisionalText = x})

getEdProvisionalDirContents :: SystemStateRef -> UI (Maybe [FilePath])
getEdProvisionalDirContents ssr =
  do x <- getAct ssr actUiState edProvisionalDirContents
     return x
putEdProvisionalDirContents
  :: SystemStateRef -> Maybe [FilePath] -> UI ()
putEdProvisionalDirContents ssr x =
  putAct ssr actUiState (\a -> a {edProvisionalDirContents = x})

getEdTextLastSaved :: SystemStateRef -> UI (Maybe String)
getEdTextLastSaved ssr =
  do x <- getAct ssr actUiState edTextLastSaved
     return x
putEdTextLastSaved :: SystemStateRef -> Maybe String -> UI ()
putEdTextLastSaved ssr x =
  putAct ssr actUiState (\a -> a {edTextLastSaved = x})

gets16program :: SystemStateRef -> UI S16program
gets16program ssr =
  do x <- getAct ssr actUiState s16program
     return x
puts16program :: SystemStateRef -> S16program -> UI ()
puts16program ssr x =
  putAct ssr actUiState (\a -> a {s16program = x})

getPxPerLine :: SystemStateRef -> UI Int
getPxPerLine ssr = do
  i <- getAct ssr actUiState pxPerLine
  return i
putPxPerLine :: SystemStateRef -> Int -> UI ()
putPxPerLine ssr i =
  putAct ssr actUiState (\a -> a {pxPerLine = i})

getCurrentModule :: SystemStateRef -> UI (Maybe S16module)
getCurrentModule ssr = do
  p <- getAct ssr actUiState s16program
  i <- getAct ssr actUiState s16CurrentMod
  case 0 <= i && i < length p of
    False -> return Nothing
    True -> return (Just (p !! i))
putCurrentModule :: SystemStateRef -> S16module -> UI ()
putCurrentModule ssr m = do
  p <- getAct ssr actUiState s16program
  i <- getAct ssr actUiState s16CurrentMod
  case 0 <= i && i < length p of
    False -> return ()
    True -> putAct ssr actUiState
              (\a -> a {s16program = setAtIndex i m p})
--       (\a -> a {s16program = take i p ++ [m] ++ drop (i+1) p})

showProgram :: SystemStateRef -> UI String
showProgram ssr = do
  p <- getAct ssr actUiState s16program
  i <- getAct ssr actUiState s16CurrentMod
  let f i j m = describeModule j (i==j) m
  let xs =
        show (length p) ++ " modules\n"
        ++ concat [f i j m | (j,m) <- zip [0..] p]
  return xs
    
newModule :: SystemStateRef -> UI ()
newModule ssr = do
  p <- getAct ssr actUiState s16program
  let p' = p ++ [emptyModule]
  let i = length p
  putAct ssr actUiState
    (\s -> s {s16program = p', s16CurrentMod = i})

{- deprecated
--  , selectedFileName    :: Maybe FilePath -- deprecated
getSelectedFileName :: SystemStateRef -> IO (Maybe FilePath)
getSelectedFileName ssr =
  do x <- getAct ssr actUiState selectedFileName
     return x
putSelectedFileName :: SystemStateRef -> Maybe FilePath -> IO ()
putSelectedFileName ssr x =
  putAct ssr actUiState (\a -> a {currentFileName = x})
-}

----------------------------------------------------------------------
-- Emulator state
----------------------------------------------------------------------

data EmuState = EmuState
  { procLoadModule    :: Maybe [String]
  , resetArchState    :: ArchState
--  , procStatus        :: ProcStatus
  , procStatusDisplay :: Element
  , instrCountDisplay :: Element
--  , ctlPause            :: IORef Bool      -- processor Pause clicked
  , memHighWaterMark  :: Int             -- how much to display
  , pcDisplay         :: (Element,Element)
  , irDisplay         :: (Element,Element)
  , adrDisplay        :: (Element,Element)
  , datDisplay        :: (Element,Element)
  , opDisplay         :: Element           -- opcode of instruction
  , argsDisplay       :: Element           -- arguments of instruction
  , ccDisplay         :: Element           -- condition code
  , fmtDisplay        :: Element           -- instruction format
  , eff1Display       :: Element           -- effect of instruction
  , eff2Display       :: Element           -- effect of instruction
  , regFileDisplay    :: [(Element,Element)]
  , memString         :: [String]        -- a string for each mem loc
  , memView1Display   :: Element         -- view 1 to show memory
  , memView2Display   :: Element         -- view 2 to show memory
  , emuListingDisplay :: Element
  , highlightedMem    :: [EmuEffect]
  , highlightedRegs   :: [EmuEffect]   -- effects on register file
  , emuAsmListing     :: [String]
  , highlightedAsmLine :: Maybe (Int, String)
  }

-- used in processor pane for clearing tags to indicate words
-- that were updated or fetched

data MemTag = Updated Word16 | Fetched Word16

----------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------

setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex i v xs =
  take i xs ++ (v : drop (i+1) xs)


----------------------------------------------------------------------
-- Deprecated, on the way out

{-

-- old parts of  data CPUstate =
    , procDelayTime   :: Int
    , accessColorLog  :: [MemTag]
    , accessColorLogRF :: [MemTag]   -- Jan 31 2015

    , procIOTextBuf   :: Maybe TextBuffer
    , procInputBuffer :: String

    , procAsmTextBuf  :: Maybe TextBuffer
    , procAsmTextView  :: Maybe TextView
    , procAsmExecLine :: Int
    , procLineAddrTable :: Maybe LineAddrTable

    , traceTextBuf     :: Maybe TextBuffer
    , dumpTextBuf      :: Maybe TextBuffer
    , logTextBuf    :: Maybe TextBuffer


-- An operation may need mutually exclusive access to the data
-- structures required for an activity.  Two functions are provided
-- for each activity, and they are held in an MVAccess data structure
-- of type MVAccess a b, where a is the type of the global system
-- state (which will be the same for every activity), and b is the
-- type of the activity's own state (which will, in general, be
-- different for each activity).

data MVAccess a b = MVAccess (a -> MVar b) (a -> MVar b -> a)

-- An MVAccess is defined for each activity, by applying the MVAccess
-- constructor to (1) the activity (which serves as a function from
-- the SystemState to the current value of the activity state) and (2)
-- a function that updates the system state with a new value of the
-- activity.

activityUiState :: MVAccess SystemState UiState
activityUiState = MVAccess actUiState (\s a -> s {actUiState=a})

activityEmuState :: MVAccess SystemState EmuState
activityEmuState = MVAccess actEmuState (\s a -> s {actEmuState=a})

--  , dirContents         :: Maybe [FilePath]
getDirContents :: SystemStateRef -> IO (Maybe [FilePath])
getDirContents ssr =
  do x <- getAct ssr actUiState dirContents
     return x
putDirContents :: SystemStateRef -> [FilePath] -> IO ()
putDirContents ssr xs =
  putAct ssr actUiState (\a -> a {dirContents = Just xs})

-}
