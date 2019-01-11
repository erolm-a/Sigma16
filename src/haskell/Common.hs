-- FIXME: Ambiguity error at line 563
{-# LANGUAGE AllowAmbiguousTypes #-}
module Common where

-- Defines data structures that are common throughout the program

import Control.Monad.State
import System.FilePath
import Data.Word
import Data.Bits
import Graphics.UI.Threepenny.Core

import Arithmetic
import Architecture



data BreakpointSpec
  = BreakpointClear
  | BreakpointNinstr Int
  deriving (Read, Show)

----------------------------------------------------------------------
-- Language for breakpoints
----------------------------------------------------------------------

data BPbool
  = BPlt BPnum BPnum
  | BPle BPnum BPnum
  | BPeq BPnum BPnum
  | BPge BPnum BPnum
  | BPgt BPnum BPnum
  | BPnot BPbool
  | BPand BPbool BPbool
  | BPor BPbool BPbool
  | BPxor BPbool BPbool
  | BPsetMem Word16
  deriving (Read, Show)

data BPnum
  = BPconst Word16
  | BPhex String
  | BPpc
  | BPsetRF 
  deriving (Read, Show)




----------------------------------------------------------------------
-- Controller state
----------------------------------------------------------------------

-- The controller needs the object code and assembly listing to
-- perform a boot, but the user interface state for these items is
-- represented differently for the text interface and the graphical
-- interface (StateT vs mutable state).  Therefore the boot operation
-- takes the object and listing as arguments, and the actual
-- operations to obtain those values are defined in the respective
-- main programs.

-- The interaction maintains a state for the user interface, which is
-- separate from the architecture state maintained by the emulator.
-- Each element is represented with a triple (the value, whether it
-- was fetched, whether it was modified).

-- The user interface maintains its own state, with copies of the
-- values of the control registers, as well as a list of recent
-- accesses to the register file and memory.  This information can be
-- used to output changes to the state.  To print the full contents of
-- the register file or memory, the architecture state must be
-- queried.

data CtlState = CtlState
  { ctlQuit        :: Bool         -- have received command to quit
  , ctlS16program  :: S16program   -- source and/or object code
  , ctlArchState   :: ArchState    -- machine registers and memory
  }

initCtlState :: CtlState
initCtlState = CtlState
  { ctlQuit        = False
  , ctlS16program  = initS16program
  , ctlArchState   = initArchState
  }



----------------------------------------------------------------------
-- Sigma16 program
----------------------------------------------------------------------

-- A program consists of a set of modules and maybe an executable
-- produced by the linker.  The modules will appear in order in the
-- executable, and they are referred to by their index in the list of
-- modules.

type S16program = [S16module]
initS16program :: S16program
initS16program = []

type Used = Bool
type Modified = Bool

----------------------------------------------------------------------
-- Sigma16 program module
----------------------------------------------------------------------

-- Data structures and functions that are used in common by several
-- components of the system

-- There are two central data structures used by the controller: the
-- uier interface state (type UIstate), and the representation of a
-- Sigma16 module (type Module).  An S16 program may contain several
-- modules, each containing some assembly language source code and
-- object code.  Optionally, there may be a linked module created by
-- the linker.  If an assembly language program doesn't use any
-- external names, it does not need to be linked, but can be executed
-- directly.  In this case, there would be just one module and there
-- would not be any executable module.  If there are several modules,
-- these are kept in a list.  The linker will put the modules together
-- in the order they appear in this list.  The modules in the list are
-- identified by their index in the list, starting from 0.  Module 0
-- will appear first in the executable, so it should be the main
-- program.

-- The input to the assembler is a string containing the source code,
-- as well as an indication of where the source code came from (a
-- file, or an editing window).  The output of the assembler consists
-- of several pieces of information collected together in an
-- AsmModule.  This contains the source code, assembly listing, error
-- messages, and the information needed to construct the object
-- module.

-- Make a string that identifies a module
identS16Module :: S16module -> String
identS16Module m =
  (case s16modName m of
     Nothing -> "   "
     Just mname -> mname)
  ++ "  " ++
  (case s16modFilePath m of
     Nothing -> ""
     Just p -> takeFileName p)
  ++ "  " ++
  (case s16modAsmSrc m of
    Nothing -> ""
    Just xs -> ("(" ++ show (length (lines xs)) ++ " lines)"))
    

data S16module =
  S16module
    { s16modName     :: Maybe String      -- name of module
--    , s16modDir      :: Maybe FilePath    -- directory containing mod
    , s16modFilePath     :: Maybe FilePath    -- file name relative to dir
    , s16modAsmSrc   :: Maybe String      -- source code
    , s16modMetadata :: Maybe AsmMetadata -- info about assembly
    , s16modObjCode  :: Maybe String      -- machine language
    }
  deriving Show

showModName :: Maybe String -> String
showModName Nothing = "(anonymous)"
showModName (Just xs) = xs

describeModule :: Int -> Bool -> S16module -> String
describeModule i isCurrent m =
  (if isCurrent then "* " else "  ")
  ++ padLeftToWidth 5 ("[" ++ show i ++ "] ")
  ++ showModName (s16modName m)
  ++ "\n       "
--  ++ show (length (lines (s16modAsmSrc m)))
  ++ " lines of code"
  ++ "\n       "
--  ++ (case s16modFilePath m of
--        Nothing -> "no file associated"
--        Just f ->  f)

emptyModule = S16module
    { s16modName       = Nothing
--    , s16modDir        = Nothing
    , s16modFilePath       = Nothing
    , s16modAsmSrc     = Nothing
    , s16modMetadata   = Nothing
    , s16modObjCode    = Nothing
    }

data AsmMetadata = AsmMetadata
  { s16modAsmGoodObj    :: Bool           -- is assembly successful?
  , s16modAsmListing    :: [String]       -- listing
  , s16modObjSymTbl     :: SymbolTable    -- symbols defined and used
  , s16modLineAddrTable :: LineAddrTable -- to help trace
  , s16modNumBadStmts   :: Int            -- lines with errors
  , s16modErrLocs       :: [(Int,Int)]    -- locations of errors
  }
  deriving Show

getLat :: S16module -> LineAddrTable
getLat md =
  case s16modMetadata md of
    Nothing -> []
    Just x -> s16modLineAddrTable x

getListing :: S16module -> [String]
getListing md =
  case s16modMetadata md of
    Nothing -> [""]
    Just x -> s16modAsmListing x

emptyAsmMetadata = AsmMetadata
  { s16modAsmGoodObj     = False
  , s16modAsmListing     = []
  , s16modObjSymTbl      = []
  , s16modLineAddrTable  = []
  , s16modNumBadStmts    = 0
  , s16modErrLocs        = []
  }



--  ++ padLeftToWidth 6 (show (length (lines (s16modAsmSrc m))))

-- The line address table shows which source line corresponds to an
-- address; it is used by the emulator to highlight the source line
-- currently being executed.

type LineAddrTable = [(Word16,Int)]

----------------------------------------------------------------------
-- Symbol table
----------------------------------------------------------------------

type SymbolTable = [SymbolDef]

data SymbolDef = SymbolDef
  { symName  :: String      -- the symbol
  , symWhere :: SymWhere    -- is it imported, or defined locally?
  , symExpr  :: Expr        -- its definition as an expression
  , symVal   :: Address16   -- value of the expression, initially 0
  , defLines :: [Int]       -- source lines where it's defined
  , useLines :: [Int]       -- source lines where it's used
  } deriving Show

data SymWhere
  = SymLabel           -- defined as a label
  | SymEqu             -- defined by an equ statement
  | SymImport        -- imported from another module
  deriving (Eq,Show)

showSymWhere :: SymWhere -> String
showSymWhere SymLabel  = "Lab"
showSymWhere SymEqu    = "Equ"
showSymWhere SymImport = "Imp"

lookupSym :: String -> SymbolTable -> Maybe SymbolDef
lookupSym s [] = Nothing
lookupSym s (x:xs) =
  if s == symName x
    then Just x
    else lookupSym s xs

----------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------
-- An expression denotes the value of a word.  It may be a constant,
-- name, or the result of an arithmetic operation.  Expressions are
-- either fixed or relocatable.  The difference is that if an object
-- module is linked, and its starting address is changed from 0 to k,
-- then all relocable words in its object code are incremented by
-- k.

data Expr
  = ExpStar
  | ExpSym String
  | ExpCon Constant
  | ExpSum Expr Expr
  | ExpDiff Expr Expr
  | ExpProd Expr Expr
  | ExpQuot Expr Expr
  | ExpRel Expr
  | ExpFixed Expr
  deriving Show

data ObjectWord
  = ObjFixed Word16     -- nonrelocatable
  | ObjRel Word16       -- relocatable
  deriving (Eq, Show)

data RegisterSrc = RegHex Char | RegDec String deriving Show

----------------------------------------------------------------------
-- Lexical components of assembly language
----------------------------------------------------------------------

data Regoperand = Regoperand Char deriving Show

data Val16
  = ValSym String
  | ValCon Constant
  deriving Show

data Constant
  = ConstantI Int
  | ConstantHex String
  | ConstantBits String
  deriving Show

data OperandField
  = Operand_k   Expr
  | Operand_RRR Regoperand Regoperand Regoperand
  | Operand_RX  Regoperand (Expr,Regoperand)
  | OperandBadSyntax
  | NoOperand
  deriving Show

----------------------------------------------------------------------
-- Architecture state
----------------------------------------------------------------------

data ArchState = ArchState
  { astPC              :: Word16            -- program counter
  , astIR              :: Word16            -- instruction register
  , astAD              :: Word16            -- address register
  , astDAT             :: Word16            -- address register
  , astRF              :: RegFile           -- register file
  , astMem             :: PrimaryMemory     -- primary memory
  , astProcStatus      :: ProcStatus
  , astInstrCount      :: Int               -- how many instructions
  , astSetInstrCount   :: String -> UI ()  -- set value in em count
  , astWriteOperation  :: String -> UI ()   -- write to I/O buffer
  , astReadOperation   :: UI String         -- read from Input buffer
  , astSetInputOperation  :: String -> UI ()  -- set input buffer
  , uiIObuffer         :: String            -- GUI text element for IO
  , uiInputBuffer      :: String            -- GUI element for inpu
  , astEffects         :: [EmuEffect]       -- actions performed
  , astInstruction     :: Instruction       -- actual instruction
  , astAddrExInstr     :: Word16            -- address executed instr
  }

initArchState :: ArchState    -- used to be initAST
initArchState = ArchState
  { astPC          = 0
  , astIR          = 0
  , astAD          = 0
  , astDAT          = 0
  , astRF          = initRegFile
  , astMem         = initMem 16
  , astProcStatus  = Reset
  , astInstrCount  = 0
  , astSetInstrCount = \xs -> return ()  -- dummy
  , astReadOperation = return ""          -- dummy
  , astSetInputOperation = \xs -> return ()   -- dummy
  , astWriteOperation = \xs -> return ()  -- dummy
  , uiIObuffer        = ""
  , uiInputBuffer     = ""
  , astEffects     = []
  , astInstruction = (Instruction (UnknownInstruction 0) NopRRR [])
  , astAddrExInstr  = 0
  }

data ProcStatus
  = Reset        -- regs, mem set to 0
  | Ready        -- program booted
  | Running      -- executing instructions
  | Halted       -- executed halt (trap 0,0,0)
  | Paused       -- pause button clicked while running
  | Blocked      -- waiting on input
  | Breakpoint   -- halted on breakpoint
  deriving Read

-- Use explicit show instance to control what is shown in the
-- emulator status value display

instance Show ProcStatus where
  show Reset      = "Reset"
  show Ready      = "Ready"
  show Running    = "Run"
  show Halted     = "Halt"
  show Paused     = "Pause"
  show Blocked    = "Block"
  show Breakpoint = "Break"

----------------------------------------------------------------------
-- Effects

----------------------------------------------------------------------
-- Generic memory
----------------------------------------------------------------------

-- A generic memory with type (MemTree a b) is a set of locations of
-- type b that are accessed by an address of type a.  It can be used
-- to form a memory, cache, or register file.  It is represented as a
-- binary tree, so fetching takes logarithmic time and storing takes
-- logarithmic space and time.

data MemTree a b
  = MemLocation !b
  | MemNode !(MemTree a b) !(MemTree a b)
  deriving Show

-- Build a balanced memory tree with 2^k cells all initialized to x.

initMemTree :: Integral a => a -> b -> MemTree a b
initMemTree k x
  | k<=0  =  MemLocation x
  | k>0   =  let y = initMemTree (k-1) x
             in MemNode y y

-- showMem converts a MemTree to a string, using a formatting function
-- It avoids using ++ or concat, for efficiency.  Preconditions: the
-- tree is balanced and contains 2^s locations.  The local function f
-- formats one cell (a=address,x=value), conses it onto continuation
-- k. The MemTree should be balanced and contain 2^s locations where s
-- is the size parameter.  a is starting address for the cells in this
-- tree.  k is continuation: string for all cells to the right of this
-- tree.

showMemTree
  :: Integral a
  => (a -> b -> String -> String)
  -> a -> a -> MemTree a b -> String -> String
showMemTree f s a (MemLocation x) k = f a x k
showMemTree f s a (MemNode x y) k
  = showMemTree f s' a x ('\n' : showMemTree f s' (a + 2^s') y k)
  where s' = s-1

memTreeFetch :: (Integral a, Bits a) => a -> a -> MemTree a b -> b
memTreeFetch k addr mem =
  let f :: Integral a => a -> MemTree a b -> b
      f j m =
        case m of
          MemLocation x -> x
          MemNode t0 t1 ->
            f (j-1) (if testBit addr (fromIntegral j) then t1 else t0)
  in f (k-1) mem

-- Store a value x::b at address a::a in a memory with 2^s
-- locations. To handle an s-bit address, use memTreeStore s addr mem
-- a x.  The looper function f uses bits from position s-1 down to 0.
-- Returns (y, mem') where y is the old value at the location with
-- address a, and mem' is the new memory with x stored at address a.

memTreeStore
  :: (Integral a, Bits a)  -- allows testbit to be used on address
  => a                 -- size: memory should contain 2^s locations
  -> MemTree a b       -- old memory state
  -> a                 -- address
  -> b                 -- value to store
  -> (b, MemTree a b)  -- (old value, new memory state)
memTreeStore k mem a x =
  let f j m =
        case m of
          MemLocation y ->
            if j == (-1)
              then (y, MemLocation x)
              else error "address size doesn't match memory size"
          MemNode t0 t1 ->
            case testBit a j of
              False ->
                let (y,t0') =  f (j-1) t0
                in (y, MemNode t0' t1)
              True ->
                 let (y,t1') = f (j-1) t1
                 in (y, MemNode t0 t1')
  in f (fromIntegral (k-1)) mem

----------------------------------------------------------------------
-- Register file
----------------------------------------------------------------------

-- There are 16 registers, each containing 16 bits.  A 4-bit address
-- selects a register.  The address is represented as Word8, a
-- primitive type in Haskell, because Haskell (as well as typical
-- machines) does not directly support Word4.

type RegFile = MemTree RegAddress Word16

-- Build a register file with all registers set to 0.

initRegFile :: RegFile
initRegFile = initMemTree 4 0

-- Fetch a word from register file at address a::RegAddress.  Force
-- register 0 to return 0; R0 is a constant 0.

fetchRF :: RegFile -> RegAddress -> Word16
fetchRF rf a =
  if a==0
    then 0
    else memTreeFetch 4 (fromIntegral a) rf

-- Load x::Word16 into a register file at location a::RegAddress.
-- Return the (old register value, new register file).

loadRF :: RegFile -> RegAddress -> Word16 -> (Word16, RegFile)
loadRF = memTreeStore 4

showRegFile :: RegFile -> String
showRegFile rf = showMemTree showRegFileLocation 4 0 rf []

-- Show a register: address followed by value

showRegFileLocation :: RegAddress -> Word16 -> String -> String
showRegFileLocation a x k =
  showRegNum a ++ " " ++ inthexok 4 x ++ k

-- Show a register address with R preceding the number.  Unused???

showRegNum :: (Integral a, Show a) => a -> String
showRegNum i =
  let xs = 'R' : show i
  in if i < 10 then ' ':xs else xs


----------------------------------------------------------------------
-- Primary memory
----------------------------------------------------------------------

-- The address and contents of a memory location are both of type
-- Word16. The primary memory contains 2^memSize locations. The value
-- of memSize is set here; it could be changed to allow memSize to be
-- set dynamically.  Whether that's worthwhile depends on how much it
-- would affect speed.

memSize :: MemAddress
memSize = 16
type PrimaryMemory = MemTree MemAddress Word16

-- Build a memory with 2^k words, all initialized to 0

initMem :: MemAddress -> PrimaryMemory
initMem k = initMemTree k 0

-- Fetch a word from memory at address a :: MemAddress.

fetchMem :: PrimaryMemory -> MemAddress -> Word16
fetchMem mem a = memTreeFetch memSize (fromIntegral a) mem

fetchMemBlock
  :: Integral a
  => PrimaryMemory     -- m = memory state
  -> MemAddress        -- a = starting address
  -> a                 -- s = size of block to fetch
  -> [Word16]          -- block of fetched data
fetchMemBlock m a s
  | s <= 0 = []
  | s > 0  = fetchMem m a : fetchMemBlock m (a+1) (s-1)


-- Store x::Word16 into a memory at location a::MemAddress.  Return
-- the (old value, new memory).

storeMem
  :: PrimaryMemory               -- old memory state
  -> MemAddress                  -- address
  -> Word16                      -- value to store
  -> (Word16, PrimaryMemory)
storeMem = memTreeStore memSize

-- Store block or words xs starting at address a into memory

storeMemBlock
  :: Integral a
  => PrimaryMemory     -- old memory state
  -> MemAddress        -- starting address
  -> [Word16]          -- block of values to store
  -> PrimaryMemory     -- new memory state
storeMemBlock m a [] = m
storeMemBlock m a (x:xs)
  = storeMemBlock (snd (storeMem m a x)) (a+1) xs

{- needs fixing
showPrimaryMemory :: PrimaryMemory -> String
-- showPrimaryMemory = showMemory showMemLocation
showPrimaryMemory = showMemTree showMemLocation
-}

-- Consider putting inthexok into continuation style too ???

showMemLocation :: Integral a => PrimaryMemory -> a -> String
showMemLocation m a =
  formatMemLocation
    (fromIntegral a)
    (memTreeFetch 16 (fromIntegral a) m)
--  word16Hex (fromIntegral a)
--   ++ (' ' : word16Hex (memTreeFetch 16 (fromIntegral a) m))
--   ++ "\n"

formatMemLocation :: Word16 -> Word16 -> String
formatMemLocation a v =
  word16Hex a ++ (' ' : word16Hex v) ++ "\n"

{-
showMemLocation :: MemAddress -> Word16 -> String -> String
showMemLocation a x k =
  inthexok 4 a ++ " " ++ inthexok 4 x ++ k
-}

----------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------

padRightToWidth :: Int -> String -> String
padRightToWidth k xs =
  xs ++ take (k - length xs) (repeat ' ')

padLeftToWidth :: Int -> String -> String
padLeftToWidth k xs =
  take (k - length xs) (repeat ' ') ++ xs

