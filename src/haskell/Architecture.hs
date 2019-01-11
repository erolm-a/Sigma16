module Architecture where

-- This module specifies the Sigma16 instruction set architecture and
-- basic system components.


import Data.Word
import Data.Bits
import Data.List

import Arithmetic

----------------------------------------------------------------------
-- Words
----------------------------------------------------------------------

type MemAddress = Word16
type RegAddress = Word8

----------------------------------------------------------------------
-- Instruction formats
----------------------------------------------------------------------

-- There are two kinds of format: the machine instruction formats, and
-- the assembly language instruction statement formats.  There are
-- three machine instruction formats: RRR, RX, EXP.  However, there is
-- a larger set of assembly language statement formats, because there
-- are special syntaxes for some instructions, and there are assembler
-- directives that aren't instructions at all.

-- Opcodes:    op, xop (if op=e), b (if op=f)
-- Registers:  d, a, b, p, q
-- Constants   disp (16 bits), k (8 bits), kp (4 bits), kq (4 bits)

-- The first word of every instruction is loaded into ir, and it
-- always has these fields; each is 4 bits

--------------------------------------------------------------------
--            Fields              Format         Pseudo format
--------------------------------------------------------------------

--    ---------------------
--    | op |  d |  a |  b |    RRR, if op<e      RRR, RR
--    ---------------------

-- If the instruction has a second word, it is loaded into adr, and it
-- has several formats, depending on op
 
--    ---------------------
--    |   xop   |    k    |    EXP, if op=e      RRRK
--    ---------------------
--    |   xop   | kp | kq |    EXP, if op=e      RRRKK
--    ---------------------
--    |   xop   |  p | kq |    EXP, if op=e      RRRRK
--    ---------------------
--    |   xop   |  p |  q |    EXP, if op=e      RRRRR
--    ---------------------
--    |       disp        |    RX,  if op=f      RX, JX
--    ---------------------

-- These are the assembly language statement formats; some
-- of them are pseudo formats that map into one of the three
-- architecture formats (RR, EXP, RX).

data InstrFormat
  = Empty
  | AsmDir  -- assembler directive
  | Con     -- constant, for data statement        k16
  | RRR     -- machine instruction format    op    d,a,b
  | RR      -- pseudo RRR omit d             op    a,b
  | EXP     -- EXP expands RRR               op,x  d,a,b
  | RRRX    -- machine instruction format    op,x  d,a,b
  | RRRK    -- machine instruction format    op,x  d,a,b,k8
  | RRRRK   -- pseudo EXP                    op,x  d,a,b,p,k4
  | RRRRR   -- pseudo EXP                    op,x  d,a,b,p,q
  | RX      -- machine instruction format    op,b  d,k16[a]
  | JX      -- pseudo RX cond jump (d)       op,b  k16[a]
  deriving Show

type InstrSize = Int

-- Return the number of words required to represent an instruction of
-- a given format

fmtSize :: InstrFormat -> InstrSize
fmtSize Empty   = 0
fmtSize AsmDir  = 0
fmtSize Con     = 1
fmtSize RRR     = 1
fmtSize RR      = 1
fmtSize RRRX    = 2
fmtSize RRRK    = 2
fmtSize RRRRK   = 2
fmtSize RRRRR   = 2
fmtSize RX      = 2
fmtSize JX      = 2

----------------------------------------------------------------------
-- Decode and show an instruction
----------------------------------------------------------------------

-- an Instruction data structure holds the decoded instruction
-- (i.e. what was actually executed, not the assembled code) and the
-- primary effect of the instruction, which is typically one or more
-- changes to registers and memory

data Instruction
  = Instruction DecodedInstruction OpSymbol [EmuEffect]
    deriving Show

data DecodedInstruction
  = RRRinstruction MemAddress RegAddress RegAddress RegAddress
  | RXinstruction MemAddress
      RegAddress RegAddress RegAddress   -- rd, ra, rb
      Word16                             -- displacement
      Word16                             -- effective address
  | EXPinstruction MemAddress
      RegAddress RegAddress RegAddress   -- rd, ra, rb
      Word8                              -- opx
      Word4                              -- p
      Word4                              -- q
  | UnknownInstruction MemAddress
  deriving Show

instructionFormat :: DecodedInstruction -> InstrFormat
instructionFormat (RRRinstruction _ _ _ _) = RRR
instructionFormat (RXinstruction _ _ _ _ _ _) = RX
instructionFormat (EXPinstruction _ _ _ _ _ _ _) = EXP
instructionFormat _ = Empty

----------------------------------------------------------------------
-- Instruction effects
----------------------------------------------------------------------

-- Effects that get a value contain the value: e.g. GetPC Word16
-- represents fetching the pc register and it contains the value.
-- Effects that set a value contain both the old and new value:
-- e.g. SetPC Word16 Word16 gives the previous and new values of the
-- pc.  When the emulator has decoded an instruction, it creates an
-- effect containing the fields of the instruction.

data EmuEffect
  = GetPC Word16                      -- PC value
  | SetPC Word16 Word16               -- PC value: old, new
  | GetIR Word16                      -- IR value
  | SetIR Word16 Word16               -- IR value: old, new
  | GetAD Word16                      -- AD value
  | SetAD Word16 Word16               -- AD value: old, new
  | GetDAT Word16                      -- Dat value
  | SetDAT Word16 Word16               -- Dat value: old, new
  | GetRF RegAddress Word16           -- regnum, val
  | SetRF RegAddress Word16 Word16    -- regnum, old, new
  | GetMem MemAddress Word16          -- address, val
  | SetMem MemAddress Word16 Word16   -- address, old, new
  | DoJump
  | NoJump
  | Halt
  | Write Word16 Word16
  | MemHighlighted MemAddress Word16  -- the address is highlighted
  | RegHighlighted RegAddress Word16  -- the register is highlighted
  | NoEffect   -- not really used, just a placeholder
  deriving Show

-- The instruction decode display shows the main effects of an
-- instruction, which are updates to key registers and memory.  Other
-- effects are not shown.  Changes to the pc are shown only for jumps.

showMainEffect :: EmuEffect -> String
showManEffect (SetPC old new) = "pc := $" ++ word16Hex new
showMainEffect (SetRF r old new) =
  "R" ++ show r ++ " := $" ++ word16Hex new
showMainEffect (SetMem a old new) =
  "mem[$" ++ word16Hex a ++ "] := $" ++ word16Hex new
showMainEffect _ = ""

{-
addEffect :: EmuEffect -> StateT CtlState IO ()
addEffect e =
  do (ast,ectl,efs) <- get
     put (ast,ectl,e:efs)
-}

padOp :: Int -> OpSymbol -> String
padOp size op =
  let xs = show op
      k = size - length xs
  in  xs ++ take k (repeat ' ')

showInstruction :: DecodedInstruction -> OpSymbol -> String
showInstruction (RRRinstruction addr d a b) op =
  word16Hex addr ++ "   " ++ padOp 6 op
     ++ "R" ++ show d ++ ",R" ++ show a ++ ",R" ++ show b
showInstruction (RXinstruction addr d a b disp ea) op =
  word16Hex addr ++ "   " ++ padOp 6 op
    ++ "R" ++ show d
    ++ "," ++ word16Hex disp ++ "[R" ++ show a ++ "]"
showInstruction _ _ = "Unknown"

-- a lot of redundancy with showInstruction ??????????????????
-- The data structure for DecodedInstruction may not be ideal...

showInstructionArgs :: DecodedInstruction -> String
showInstructionArgs (RRRinstruction addr d a b) =
     "R" ++ show d ++ ",R" ++ show a ++ ",R" ++ show b
showInstructionArgs (RXinstruction addr d a b disp ea) =
    "R" ++ show d
    ++ ",$" ++ word16Hex disp ++ "[R" ++ show a ++ "]"
showInstructionArgs _ = ""

-- Decode the instruction held in the ir and possibly the ad, so it
-- can be displayed by the user interface

-- replaces the old decodeOpSymbol from Architecture.hs

-- Given the fields of the ir, return the instruction format and
-- mnemonic

decodeOpcode
  :: Word4 -> Word4 -> Word4 -> Word4
  -> (InstrFormat,OpSymbol)
decodeOpcode ir_op ir_d ir_a ir_b
  | ir_op <= 13 = (RRR, ordered_RRR_symbols !! fromIntegral ir_op)
  | ir_op == 14 = (EXP, NopEXP)
  | ir_op == 15 = (RX,  ordered_RX_symbols !! fromIntegral ir_b)

decodeInstruction :: Word16 -> Word16 -> String
decodeInstruction ir ad =
  let (ir_op,ir_d,ir_a,ir_b) = splitword ir
      (format,mnemonic) = decodeOpcode ir_op ir_d ir_a ir_b
  in case format of
       RRR -> show mnemonic ++ " "
                ++ showDecodedRRRoperands ir_d ir_a ir_b
       RX  -> show mnemonic ++ " "
                ++ showDecodedRXoperands ir_d ir_a ad

showDecodedRXoperands
  :: RegAddress -> RegAddress -> Word16 -> String
showDecodedRXoperands d a x =
  "R" ++ show d ++ ",$" ++ inthexok 4 x
    ++ "[R" ++ show a ++ "]"

showDecodedRRRoperands
  :: RegAddress -> RegAddress -> RegAddress -> String
showDecodedRRRoperands d a b =
  "R" ++ show d ++ ",R" ++ show a ++ ",R" ++ show b

----------------------------------------------------------------------
-- Assembly language statements
----------------------------------------------------------------------

-- An OpSymbol represents a statement type in the assembly language.
-- These are either assembler directives, constants, or instruction
-- mnemonics.

data OpSymbol
  = EmptyStmt

-- Assembler directives
  | AsmMod
  | Export
  | Import
  | EquStmt
  | OrgStmt

-- Constants
  | DataStmt

-- RRR format instructions
  | Add     | Sub     | Mul     | Div    | Cmp
  | Inv16   | And16   | Or16    | Xor16  | ShiftL  | ShiftR
  | NopRRR  -- for unused RRR opcodes
  | Trap

-- EXP format instructions
  | IntOn   | IntOff  | SysOff  | TestSet
  | NopEXP  -- for unused EXP opcodes

-- RX/JX format instructions
  | Lea     | Load    | Store
  | Jump    | Jumpc0  | Jumpc1
  | Jumpgtu | Jumpleu
  | Jumpgt  | Jumple
  | Jumpeq  | Jumpne
  | Jumplt  | Jumpge
  | Jumpltu | Jumpgeu
  | Jumpovu | Jumpnovu
  | Jumpov  | Jumpnov
  | Jumpcp  | Jumpncp
  | Jal
  | NopRX  -- for unused RX opcodes
  | NopJX  -- for unused JX opcodes

  deriving Show

-- The following lists give the operation symbols for each format, in
-- order of opcode.  These are used to decode an instruction word in
-- order to give the corresponding operation symbol.

-- 0 <= op <= d
ordered_RRR_symbols :: [OpSymbol]
ordered_RRR_symbols =
  [Add,     Sub,     Mul,     Div,
   Cmp,     Inv16,   And16,   Or16,
   Xor16,   ShiftL,  ShiftR,  NopRRR,
   NopRRR,  Trap]

ordered_RX_symbols :: [OpSymbol]
-- primary opcode (op field) is 15
-- secondary opcode (sb field) is 0-15
ordered_RX_symbols =
  [Lea,     Load,    Store,   Jump,
   Jumpc0,  Jumpc1,  Jal,     NopRX,
   NopRX,   NopRX,   NopRX,   NopRX,
   NopRX,   NopRX,   NopRX,   NopRX]

ordered_JX_symbols :: [OpSymbol]
-- primary opcode is op field = 15
-- secondary opcode is sb field = 4,5
-- tertiary opcode is dst field = 0..15
ordered_JX_symbols =
  [ Jump
  , Jumpgtu
  , Jumpleu
  , Jumpgt
  , Jumple
  , Jumpeq
  , Jumpne
  , Jumplt
  , Jumpge
  , Jumpltu
  , Jumpgeu
  , Jumpovu
  , Jumpnovu
  , Jumpov
  , Jumpnov
  , Jumpcp
  , Jumpncp
  ]

ordered_EXP_symbols :: [OpSymbol]
-- primary opcode (op field) is 14
-- secondary opcode (sa++sb field) is 0-255
ordered_EXP_symbols =
  [IntOn,   IntOff,  SysOff,  TestSet]
   ++ take 256 (repeat NopEXP) -- ensure all opcodes are covered

----------------------------------------------------------------------
-- Statements and instruction set properties
----------------------------------------------------------------------

-- The assembler uses a table of type instrProperties to describe the
-- structural characteristics of each instruction.  The architecture
-- uses expanding opcodes.  A general opcode is represented as [Int]
-- where the list contains the sequence of opcode values.

type Mnemonic = String
type InstrProperties =
  ( Mnemonic     -- operation as written in assembly language
  , OpSymbol     -- statement type
  , InstrFormat  -- structure of assembly language operands
  , [Int]        -- expanding opcode
  )

instrProperties :: [InstrProperties]
instrProperties =
  [

-- Empty statements and full line comments
    ("",       EmptyStmt,  Empty, [])

-- Assembler directives
  , ("module",   AsmMod,   AsmDir,  [])
  , ("export",   Export,   AsmDir,  [])
  , ("import",   Import,   AsmDir,  [])
  , ("org",      OrgStmt,  AsmDir,  [])
  , ("equ",      EquStmt,  AsmDir,  [])

-- Constants
  , ("data",     DataStmt, Con,     [])

-- RRR format instructions
  , ("add",      Add,      RRR,     [ 0])
  , ("sub",      Sub,      RRR,     [ 1])
  , ("mul",      Mul,      RRR,     [ 2])
  , ("div",      Div,      RRR,     [ 3])
  , ("cmp",      Cmp,      RR,      [ 4])
  , ("inv",      Inv16,    RRR,     [ 5])
  , ("and",      And16,    RRR,     [ 6])
  , ("or",       Or16,     RRR,     [ 7])
  , ("xor",      Xor16,    RRR,     [ 8])
  , ("shiftl",   ShiftL,   RRR,     [ 9])
  , ("shiftr",   ShiftR,   RRR,     [10])
  , ("nopr",     NopRRR,   RRR,     [11])
  , ("nopr",     NopRRR,   RRR,     [12])
  , ("trap",     Trap,     RRR,     [13])

-- EXP format instructions
  , ("inton",    IntOn,    EXP,     [14,  0])
  , ("intoff",   IntOff,   EXP,     [14,  1])
  , ("sysoff",   SysOff,   EXP,     [14,  2])

-- RX/JX format instructions
  , ("lea",      Lea,      RX,      [15,  0])
  , ("load",     Load,     RX,      [15,  1])
  , ("store",    Store,    RX,      [15,  2])
  , ("jump",     Jump,     JX,      [15,  3,  0])
  , ("jumpc0",   Jumpc0,   RX,      [15,  4])
  , ("jumpc0",   Jumpc1,   RX,      [15,  5])
  , ("jumpgtu",  Jumpgtu,  JX,      [15,  5,  0])
  , ("jumpleu",  Jumpleu,  JX,      [15,  4,  0])
  , ("jumpgt",   Jumpgt,   JX,      [15,  5,  1])
  , ("jumple",   Jumple,   JX,      [15,  4,  1])
  , ("jumpeq",   Jumpeq,   JX,      [15,  5,  2])
  , ("jumpne",   Jumpne,   JX,      [15,  4,  2])
  , ("jumplt",   Jumplt,   JX,      [15,  5,  3])
  , ("jumpge",   Jumpge,   JX,      [15,  4,  3])
  , ("jumpltu",  Jumpltu,  JX,      [15,  5,  4])
  , ("jumpltu",  Jumpgeu,  JX,      [15,  4,  4])
  , ("jumpovf",  Jumpovu,  JX,      [15,  5,  5])
  , ("jumpnovf", Jumpnovu, JX,      [15,  4,  5])
  , ("jumpovf",  Jumpov,   JX,      [15,  5,  6])
  , ("jumpnovf", Jumpnov,  JX,      [15,  4,  6])
  , ("jumpco",   Jumpcp,   JX,      [15,  5,  7])
  , ("jumpnco",  Jumpncp,  JX,      [15,  4,  7])
  , ("jal",      Jal,      RX,      [15,  6])
  ]

-- decodeOpSymbol takes the machine instruction word, decodes the
-- opcode (which may be expanded) using the tables of ordered symbols
-- (ordered_RRR_symbols and the others).  It returns the corresponding
-- OpSymbol for the instruction.  It uses a combination of tables
-- listing the OpSymbols in the order of their opcode, and conditional
-- expressions to work out the expansion of the opcode.  It is assumed
-- that all the components of the fully expanded opcode appear in the
-- instruction word, and they don't continue into a second instruction
-- word.

decodeOpSymbol :: Integral a => a -> a -> a -> a -> OpSymbol
decodeOpSymbol ir_op ir_dst ir_sa ir_sb
  | ir_op == 15 = ordered_RX_symbols !! sb
--      if sb==3
--        then ordered_JX_symbols !! dst
--        else ordered_RX_symbols !! sb
  | ir_op == 14 = ordered_EXP_symbols !! (256*dst + sa)
  | otherwise   = ordered_RRR_symbols !! op
  where op  = fromIntegral ir_op
        dst = fromIntegral ir_dst
        sa  = fromIntegral ir_sa
        sb  = fromIntegral ir_sb

-- Extract properties of an instruction

propMne :: InstrProperties -> Mnemonic
propMne (m,_,_,_) = m

propOp :: InstrProperties -> OpSymbol
propOp (_,op,_,_) = op

propFmt :: InstrProperties -> InstrFormat
propFmt (_,_,fmt,_) = fmt

propOpcode :: InstrProperties -> [Int] -- Opcode
propOpcode (_,_,_,opc) = opc

bad_instr_props = ("illegal", Trap, RRR, [13])

----------------------------------------------------------------------
--  Representation of the architecture
----------------------------------------------------------------------

-- The machine has 16-bit words, and does not use words of other sizes
-- (no bytes, long words, etc.).  The emulator uses the type Word16
-- (represented as a Haskell Int) to represent a word as an integer in
-- the range from 0 to 2^16 - 1.  Word4 represents a 4-bit field.

{-
type Opcode = Word4
type Reg = Word4
type Addr = Word16
-}


----------------------------------------------------------------------
-- Nop
----------------------------------------------------------------------

noprResult a b sr = (False, Nothing, Nothing)
