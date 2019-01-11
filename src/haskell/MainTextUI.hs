module MainTextUI where

-- Main program with text user interface

-- temporarily, comment out everything while working on GUI
{-

import Data.List
import Data.Word

import Control.Monad.State
import System.Directory
import System.IO.Error

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language

import Arithmetic
import Architecture
import Common
import Assembler
import Linker
import Emulator
import SysEnv

{- Needs updating... ?????????????????  These are moved from
Controller to MainTextUI because the current module is handled
differently in the GUI, as part of the mutable state rather than as an
element of the StateT CtlState

srcFromCurrentMod :: StateT CtlState IO String
srcFromCurrentMod =
  do cm <- getCurrentMod
     return (s16modAsmSrc cm)

getCurrentMod :: StateT CtlState IO S16module
getCurrentMod =
  do s <- get
     let p = ctlS16program s
     return ( (s16modules p) !! (s16CurrentMod p) )

modifyCurrentMod :: (S16module->S16module) -> StateT CtlState IO ()
modifyCurrentMod f =
  do s <- get
     let p = ctlS16program s
     let ms = s16modules p
     let i = s16CurrentMod p
     let ms' = take i ms ++ (f (ms!!i) : drop (i+1) ms)
     liftIO $ putStrLn ("modifyCurrentMod " ++ show i)
     let p' = p {s16modules = ms'}
     put s {ctlS16program = p'}
     return ()

-}

-- Set the assembly source text of current module to be xs

ctlOpen :: FilePath -> StateT CtlState IO Bool
ctlOpen p =
  do liftIO $ putStrLn ("Controller: doOpen " ++ p)
     r <- liftIO $ safeReadFile p
     case r of
       Nothing -> do
         liftIO $ putStrLn "doOpen: file read failed"
         return False
       Just xs -> do
         liftIO $ putStrLn ("doOpen read this: " ++ xs)
         modifyCurrentMod (\m -> m {s16modAsmSrc = xs})
         return True

setS16AsmSrc :: String -> StateT CtlState IO ()
setS16AsmSrc xs = modifyCurrentMod (\m -> m {s16modAsmSrc = xs})

ctlBoot
  :: (EmuEffect -> StateT ArchState IO ())
  -> StateT CtlState IO ()
ctlBoot f =
  do cm <- getCurrentMod
     let obj = s16modObjCode cm
     s <- get
     let a1 = ctlArchState s
     liftIO $ putStrLn ("ctlBoot: obj = " ++ obj)
     liftIO $ putStrLn ("ctlBoot: end of obj")
     (r,a2) <- liftIO $ runStateT
       (readLoadModule f 0 (snd (parseLoadModule obj)))
       a1
     put (s {ctlArchState=a2})
     return ()

--            (a1,initEmuControl,[])
--            (a1,True,[])


----------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------

-- A command by the user has type Command, defined in Syntax.  The
-- following summary is provided in response to a help command.  Some
-- commands have a long name which is shown in parentheses.  The
-- helpText string gives for each command the full name, the
-- abbreviation (if any), the arguments (if any), and a short
-- explanation.

helpText :: String
helpText =
  "help      h         display this summary of commands\n\
  \quit      q         terminate the application\n\
  \pwd                 print working directory\n\
  \ls                  list contents of directory\n\
  \home      h         change working directory to user's home\n\
  \examples  ex        change working directory to examples\n\
  \cd            PATH  change working directory to PATH\n\
  \open      o   PATH  add file to group\n\
  \focus     fo  MOD   work on a module within group\n\
  \group     g         list file group and status\n\
  \reread    rr        re-read assembly source file\n\
  \asm       a         translate assembly to object\n\
  \save      sv        save assembly listing and object files\n\
  \link      ln        link file group\n\
  \reset     rs        reset processor registers and memory\n\
  \boot      b         load object into memory\n\
  \step      s         execute one instruction\n\
  \regs                show the registers\n\
  \mem                 show part of the memory\n\
  \break     br        set breakpoint\n\
  \run       r         execute until break or halt\n"

data Command
  = CmdHelp
  | CmdQuit
  | CmdPwd
  | CmdLs
  | CmdHome
  | CmdExamples
  | CmdCd FilePath
  | CmdOpen FilePath
  | CmdFocus String
  | CmdGroup
  | CmdReread
  | CmdAssemble
  | CmdSave
  | CmdLink
  | CmdReset
  | CmdBoot
  | CmdStep
  | CmdRegs
  | CmdMemBlock Word16 Word16
  | CmdBreak [BreakpointSpec]
  | CmdRun
  | CmdUnparseable String
  deriving (Read, Show)

----------------------------------------------------------------------
-- Interaction loop
----------------------------------------------------------------------

-- This loop runs in the IO monad, and it uses runStateT to run
-- operations in the emulator.  These operations may be short (execute
-- a single instruction) or long running (execute instructions
-- repeatedly until a condition is met).

--   runStateT op (arch state, effects) returns (r,(a,e))
--   (r,(a,e)) means (result, (new arch state, list of effects))

clearRFaccess :: StateT CtlState IO ()
clearRFaccess =
  do s <- get
--     put (s {ui_rf = []})
     return ()

clearMemaccess :: StateT CtlState IO ()
clearMemaccess =
  do s <- get
--     put (s {ui_mem = []})
     return ()

{-
interaction :: String -> String -> StateT CtlState IO ()
interaction lmname lmtext =
  do let a = initAST
     let e = []
     let temp = parseLoadS16module lmtext
     liftIO $ putStrLn "??????????????????????????????????????????????"
     liftIO $ putStrLn (concat (fst temp))
     liftIO $ putStrLn (show (snd temp))
     liftIO $ putStrLn "??????????????????????????????????????????????"
     liftIO $ putStrLn ("Reading load module " ++ lmname)
     (r,(a,ctl,e)) <- liftIO $ runStateT
       (readLoadModule 0 (snd (parseLoadModule lmtext)))
--       (a,initEmuControl,e)
       (a,True,e)
     liftIO $ putStrLn "Initial state"
     printCtlState
     liftIO $ putStrLn ""
--     a <- instructionLooper (runNinstructions 45) a  -- ????amax fix
     a <- instructionLooper (runNinstructions 104) a  -- ???? factfix
     liftIO $ putStrLn "Interaction terminating"
-}
-- Run instructions repeatedly until a termination condition becomes
-- true.

type TerminationPredicate = ArchState -> CtlState -> Bool

-- A predicate that terminates after limit instructions have
-- been executed

{-
runNinstructions :: Int -> TerminationPredicate
runNinstructions limit archstate ctlstate =
  astInstrCount ctlstate >= limit

instructionLooper
  :: TerminationPredicate
  -> ArchState
  -> StateT CtlState IO ArchState

instructionLooper f archstate =
  do archstate' <- instructionStep archstate
     uistate <- get
     case f archstate uistate of
       True -> return archstate
       False -> instructionLooper f archstate'
     
instructionStep :: ArchState -> StateT CtlState IO ArchState
instructionStep ast =
  do
     (r,(a,ectl,e)) <- liftIO $
--       runStateT executeInstruction (ast,initEmuControl,[])
       runStateT executeInstruction (ast,True,[])
     incrInstrCount
     handleEffects (reverse e)
     s <- get
--     let (ir,iru,irm) = ui_ir s
--     let (ad,adu,adm) = ui_ad s
     liftIO $ putStr (show (astInstrCount s) ++ ".  ")

--     liftIO $ putStrLn (showInstruction (ui_instr s))
     printAccesses
     printCtlState
     clearRFaccess
     clearMemaccess
     return a
-}


--     liftIO $ putStrLn (decodeInstruction ir ad)
--     liftIO $ putStrLn (show (ui_instr s))
--     liftIO $ putStrLn (show e)  -- useful for test & debug


----------------------------------------------------------------------
-- Main program
----------------------------------------------------------------------

mainTextUI :: IO ()
mainTextUI =
  do runStateT commandLoop initCtlState
     return ()

----------------------------------------------------------------------
-- Parsing commands in textual form
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Syntax for text user interface (UItext)
----------------------------------------------------------------------

-- User interface text command
parseCmd :: String -> Command
parseCmd xs =
  case (parse commandParser "" xs) of
    Left err -> CmdUnparseable (show err)
    Right x -> x

commandParser :: Parser Command
commandParser =
  do skipWhiteSpace
     cmdHelp <|> cmdQuit
     <|> cmdPwd <|> cmdLs <|>cmdHome <|> cmdExamples <|> cmdCd
     <|> cmdOpen
     <|> cmdExamples
     <|> cmdAssemble <|> cmdBoot
     <|> try cmdRegs <|> cmdMemBlock
     <|> cmdStep <|> cmdRun
     <|> cmdUnparseable

cmdHelp :: Parser Command
cmdHelp =
  do string "h"  <|> string "help" <|> string "?"
     return CmdHelp

-- ensure that there isn't anything more on the input line

noMore :: Parser ()
noMore =
  do skipWhiteSpace
     eol
     return ()

cmdQuit :: Parser Command
cmdQuit =
  do char 'q'
     return CmdQuit

cmdPwd :: Parser Command
cmdPwd =
  do string "pwd"
     return CmdPwd

cmdLs :: Parser Command
cmdLs =
  do string "ls"
     return CmdLs

cmdHome :: Parser Command
cmdHome =
  do string "home"
     return CmdHome

cmdExamples :: Parser Command
cmdExamples =
  do string "examples"
     return CmdExamples

cmdCd :: Parser Command
cmdCd =
  do string "cd"
     skipWhiteSpace
     xs <- many filePathChar
     return (CmdCd xs)

cmdOpen :: Parser Command
cmdOpen =
  do string "open"
     skipWhiteSpace
     xs <- many filePathChar
     return (CmdOpen xs)

filePathChar :: Parser Char
filePathChar =
  letter <|> digit <|> oneOf  ['_', '.', '/']

cmdAssemble :: Parser Command
cmdAssemble =
  do char 'a'
     return CmdAssemble

cmdBoot :: Parser Command
cmdBoot =
  do string "boot"
     return CmdBoot

cmdStep :: Parser Command
cmdStep =
  do char 's'
     return CmdStep

cmdRegs :: Parser Command
cmdRegs =
  do string "regs"
     return CmdRegs

cmdMemBlock :: Parser Command
cmdMemBlock =
  do string "mem"
     return (CmdMemBlock 0 8)  -- ??? temp hack, should parse args

cmdRun :: Parser Command
cmdRun =
  do string "run"
     return CmdRun

cmdUnparseable :: Parser Command
cmdUnparseable =
  do xs <- many anyChar
     return (CmdUnparseable xs)



----------------------------------------------------------------------
-- Interactive command loop
----------------------------------------------------------------------

commandLoop :: StateT CtlState IO ()
commandLoop =
  do cmd <- getCommand
     continue <- executeCommand cmd
     case continue of
       False -> return ()
       True -> commandLoop

----------------------------------------------------------------------
-- Process one command
----------------------------------------------------------------------

-- The command returns True iff the command loop should continue

getCommand :: StateT CtlState IO Command
getCommand =
  do liftIO $ putStr "$ "
     xs <- liftIO getLine
     let cmd = parseCmd xs
     return cmd

executeCommand :: Command -> StateT CtlState IO Bool
executeCommand cmd =
  do liftIO $ putStrLn ("Parsed cmd: " ++ show cmd)
     case cmd of
       CmdHelp -> liftIO $ putStr helpText >> return True
       CmdQuit -> liftIO $ putStrLn "Quitting" >> return False
       CmdPwd -> liftIO ctlPwd >>= \p -> liftIO (putStrLn p)
         >> return True
       CmdLs -> liftIO ctlLs >>= \xs ->
         liftIO $ putStrLn (concat (intersperse "\n" xs))
         >> return True
       CmdExamples ->
         do d <- liftIO $ getInstDir
-- Need to fix this, getInstDir returns Maybe ...       
--            liftIO $ putStrLn ("Examples - installation dir = " ++ d)
            return True
       CmdCd path ->
         do x <- liftIO (ctlCd path)
            case x of
              False -> do liftIO $ putStrLn ("cannot cd to " ++ path)
                          return ()
              True -> return ()
            return True
       CmdOpen path ->
         do liftIO $ putStrLn ("file " ++ path)
            ctlOpen path
            return True
       CmdAssemble ->
         do ctlAsm
            return True
       CmdBoot ->
         do ctlBoot ignoreEffect
            return True
--       CmdStep -> ctlStep ignoreEffect ignoreInstruction
       CmdStep -> ctlStep printEffect printInstruction
         >> ctlGetRegs
         >>= \rs -> liftIO (printRegs rs)
         >> return True
       CmdRun -> ctlRun ignoreEffect printInstruction
         >> return True
       CmdRegs -> ctlGetRegs >>= \rs -> liftIO (printRegs rs)
         >> return True
       CmdMemBlock a b -> ctlGetMemBlock a b
         >>= \ms -> liftIO (printMemBlock a b ms)
         >> return True
       CmdUnparseable xs ->
         liftIO $ putStrLn "invalid command" >> return True

printEffect :: EmuEffect -> StateT ArchState IO ()
printEffect e = liftIO (putStrLn (show e))

printInstruction :: Instruction -> StateT ArchState IO ()
printInstruction i = liftIO (putStrLn (show i))

printRegs :: (Word16,Word16,Word16,RegFile) -> IO ()
printRegs (pc,ir,ad,rf) =
  do putStrLn ("pc = " ++ word16Hex pc)
     putStrLn ("ir = " ++ word16Hex ir)
     putStrLn ("ad = " ++ word16Hex ad)
     return ()
--     liftIO $ putStrLn ("pc = " ++ word16Hex (astPC a))

printMemBlock :: Word16 -> Word16 -> [(Word16,Word16)]-> IO ()
printMemBlock a b ms =
  do putStrLn ("mem = " ++ show ms)
     return ()

{-
showInstructionEffect :: [EmuEffect] -> String
showInstructionEffect es = concat (map f es)
  where f (Executed i) = showInstruction i
        f _ = ""
-}


----------------------------------------------------------------------
-- Deprecated, temp testing
----------------------------------------------------------------------

{-
tester :: IO ()
tester =
  do putStrLn "main starting"
     let asmSrcFileName = "../Sigma16/Examples/Recursion/Factorial.asm.txt"
     putStrLn "*********************************************"
     asmSrc <- readFile asmSrcFileName
     putStrLn asmSrc
     putStrLn "*********************************************"
     let srclines = lines asmSrc
     let (loc, symtab, codelines, listing,
            nerrs, errlocs, lat, obj) = asm srclines
     putStrLn (concat listing)
     putStrLn "*********************************************"
     putStrLn obj
     putStrLn "*********************************************"
     let foo = parseLoadModule obj
     putStrLn (show foo)
     putStrLn "*********************************************"
     runStateT (interaction "fact" obj) initUIstate
     putStrLn "main terminating"
-}

{-
run :: String -> String -> IO ()
run lmname lmtext =
  do runStateT (interaction lmname lmtext) initUIstate
     return ()
-}

-}
