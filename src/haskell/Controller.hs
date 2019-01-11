module Controller where


import Arithmetic
import Architecture
import Common
import Assembler
import Linker
import Emulator

import System.Environment
import System.Directory
import System.IO.Error
import Control.Monad.State
import Data.Word
import Data.List


----------------------------------------------------------------------
-- Controller
----------------------------------------------------------------------

-- The Controller is a generic user interface to the components of the
-- Sigma16 system, including the assembler, linker, and emulator.  It
-- defines the requests a user can make, and the information provided
-- by the system in response.  The way this dialogue is carried out is
-- defined separately by a user interface; there may be a text
-- interface as well as one or more graphical user interfaces.


-- saveEffects :: [EmuEffect] -> StateT CtlState IO ()
-- saveEffects efs =
--   do s <- get
--      put (s {uiEffects = efs})
--      return ()



-- showS16ModName :: S16module -> String
-- showS16ModName md = s16modName md

showObjsymtbl :: S16module -> [String]
showObjsymtbl md =
  case s16modMetadata md of
    Nothing -> [""]
    Just x -> showSymTab (s16modObjSymTbl x)

showAsmlisting :: S16module -> [String]
showAsmlisting md =
  case s16modMetadata md of
    Nothing -> [""]
    Just x -> s16modAsmListing x

showObjcode :: S16module -> String
showObjcode md =
  case s16modObjCode md of
    Nothing -> ""
    Just x -> x

----------------------------------------------------------------------
-- Actions performed by the controller in response to command
----------------------------------------------------------------------

-- The user interface accesses system information via this group of
-- functions, rather than performing OS requests directly.  These
-- operations catch IO exceptions if necesseary.  These functions do
-- not print out the resulting information; the user interface should
-- do that.

ctlPwd :: IO String
ctlPwd =
  do path <- getCurrentDirectory
     return path

ctlLs :: IO [FilePath]
ctlLs =
  do path <- ctlPwd
     y <- liftIO $ tryIOError (listDirectory path)
     return $ case y of
       Left _ -> []
       Right xs -> xs


-- Set working directory (cd); return True iff successful
ctlCd :: FilePath -> IO Bool
ctlCd dest =
  do path <- ctlPwd
--     liftIO $ putStrLn ("ctlcd: pwd = " ++ path)
--     liftIO $ putStrLn ("ctlcd: dest = " ++ dest)
     y <- liftIO $ tryIOError (setCurrentDirectory dest)
--     liftIO $ putStrLn ("ctlcd: " ++ path)
     return $ case y of
       Left _ -> False
       Right _ -> True


{-
ctlAsm :: StateT CtlState IO ()
ctlAsm =
  do cm <- getCurrentMod
     liftIO $ putStrLn ("ctlAsm have cm")
     liftIO $ putStrLn ("src = " ++ s16modAsmSrc cm)
     liftIO $ putStrLn ("src = " ++ s16modAsmSrc cm)
     let (flc,st,codelines,lst,nerrs,errlocs,lat,obj)
           = asm (lines (s16modAsmSrc cm))
     liftIO $ putStrLn ("ctlAsm lst = " ++ concat lst)
     modifyCurrentMod (\m ->
       m {s16modLineAddrTable=lat, s16modNumBadStmts=nerrs,
          s16modErrLocs=errlocs,
          s16modAsmListing=lst,s16modObjSymTbl=st,
          s16modObjCode=obj})
     liftIO $ putStrLn ("\n\nObject =\n" ++ obj)
     return ()
-}


ctlStep
  :: (EmuEffect -> StateT ArchState IO ())    -- f handles effect
  -> (Instruction -> StateT ArchState IO ())  -- g handles instruction
  -> StateT CtlState IO ()
ctlStep f g =
  do s <- get
     let a1 = ctlArchState s
     (r,a2) <- undefined --
--  liftIO $ runStateT (executeIfRunning f g) a1
 -- ???????? Controller is out of date; changing to avoid errors
 -- while changing from IO to UI... Dec 29 2017
     put (s {ctlArchState=a2})
     return ()

ctlRun
  :: (EmuEffect -> StateT ArchState IO ())    -- f handles effect
  -> (Instruction -> StateT ArchState IO ())  -- g handles instruction
  -> BPbool
  -> StateT CtlState IO ()
ctlRun f g bpexp =
  do s <- get
     let a1 = ctlArchState s
     (r,a2) <- undefined
 --  liftIO $ runStateT (run f g (Just bpexp)) a1
 -- ???????? Controller is out of date; changing to avoid errors
 -- while changing from IO to UI... Dec 29 2017
     put (s {ctlArchState=a2})
     return ()

{-
ctlRun
  :: (EmuEffect -> StateT ArchState IO ())    -- f handles effect
  -> (Instruction -> StateT ArchState IO ())  -- g handles instruction
  -> StateT CtlState IO ()
  do s <- get
     let a1 = ctlArchState s
     (r,a2) <- liftIO $ runStateT executeInstruction (a1,True,[])
     put (s {ctlArchState=a2, uiEffects=e})
     case r of
       False -> return ()
       True -> ctlRun
-}

{-
ctlGetEffects :: StateT CtlState IO [EmuEffect]
ctlGetEffects =
  do s <- get
     let efs = uiEffects s
     return efs
-}


ctlGetRegs :: StateT CtlState IO (Word16, Word16, Word16, RegFile)
ctlGetRegs =
  do s <- get
     let a = ctlArchState s
     return (astPC a, astIR a, astAD a, astRF a)

ctlGetMemBlock
  :: Word16 -> Word16
  -> StateT CtlState IO [(Word16,Word16)]
ctlGetMemBlock a b =
  do s <- get
     let ast = ctlArchState s
     let m = astMem ast
     return [(a, fetchMem m a) | a <- [0..15]]



----------------------------------------------------------------------
-- Interacting with the operating system
----------------------------------------------------------------------

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path =
  do x <- tryIOError (readFile path)
     return $ case x of
       Left _ -> Nothing
       Right xs -> Just xs

-- This may not be reliable.  If using ghci, it returns the
-- location of ghci...

{-  old, replaced by getInstDir in SysEnv
getInstallationDir :: IO String
getInstallationDir =
  do x <- getExecutablePath
     return x
     -}

----------------------------------------------------------------------
-- Operations on user interface state
----------------------------------------------------------------------


incrInstrCount :: StateT ArchState IO ()
incrInstrCount =
  do s <- get
     put (s {astInstrCount = astInstrCount s + 1})

printCtlState :: StateT CtlState IO ()
printCtlState =
  do s <- get
     let f (v,u,m)
           = word16Hex v
               ++ if u then " U" else "  "
               ++ if m then "M" else " "
--     liftIO $ putStrLn ("  pc = " ++ f (ui_pc s))
--     liftIO $ putStrLn ("  ir = " ++ f (ui_ir s))
--     liftIO $ putStrLn ("  ad = " ++ f (ui_ad s))
     return ()

-- Return list of decoded instructions found in list of effects

-- getExecutedInstructions :: StateT CtlState IO [DecodedInstruction]
-- getExecutedInstructions =
--   do s <- get
--      return (concat (map getInstructionEffect (uiEffects s)))

-- If an effect is an executed instruction; show it, but ignore other
-- kinds of effect

-- getInstructionEffect :: EmuEffect -> [DecodedInstruction]
-- getInstructionEffect (Executed i) = [i]
-- getInstructionEffect _ = []


printAccesses :: StateT CtlState IO ()
printAccesses =
  do s <- get
--     liftIO $ putStrLn
--       (concat (intersperse "\n"
--         (reverse (map showRFaccess (ui_rf s)))))
--     liftIO $ putStrLn
--       (concat (intersperse "\n"
--         (reverse (map showMemaccess (ui_mem s)))))
     return ()

showRFaccess :: (RegAddress, Word16, Used, Modified) -> String
showRFaccess (a,x,u,m) =
  "  R" ++ show a ++ " = " ++ word16Hex x ++ " "
     ++ (if u then "U" else "")
     ++ (if m then "M" else "")

showMemaccess :: (MemAddress, Word16, Used, Modified) -> String
showMemaccess (a,x,u,m) =
  "  mem[" ++ word16Hex a ++ "] = " ++ word16Hex x ++ " "
     ++ (if u then "U" else "")
     ++ (if m then "M" else "")


----------------------------------------------------------------------
-- Operations on modules
----------------------------------------------------------------------

showFilePath :: S16module -> [String]
showFilePath md =
  let name = case s16modName md of
               Nothing -> "anonymous"
               Just nm -> nm
      p =  case s16modFilePath md of
               Nothing -> ""
               Just path -> path
  in [p]

--  case s16modFilePath md of
--    Nothing -> ["No file associated with module"]
--    Just xs -> ["File " ++ xs]


showS16module :: S16module -> [String]
showS16module md =
  [take 72 (repeat '-')]
  ++ ["Assembly module:"]
  ++ showFilePath md
--  ++ showAsmsrc md
--  ++ [s16modName md]
--  ++ showObjsymtbl md
  ++ showAsmlisting md
  ++ [showObjcode md]
  ++ [take 72 (repeat '-')]

