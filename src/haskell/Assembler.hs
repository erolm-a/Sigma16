----------------------------------------------------------------------
-- Sigma16
-- John O'Donnell, 2018.  See README.txt and LICENSE.txt
----------------------------------------------------------------------

module Assembler where

-- This module defines an assembler that translates the assembly
-- language into machine language.  Unit tests are defined in module
-- AsmTest.

import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Word
import Data.Bits
import Data.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language

import Common
import Arithmetic
import Architecture

----------------------------------------------------------------------
-- Interface to the assembler
----------------------------------------------------------------------

-- The assemble function is the top level interface to the assembler.
-- It takes an S16module, translates the source code in that module
-- (if any), and updates the module with the results of the assembly.
-- This includes a listing, symbol table, other metadata, and the
-- object code.

assemble
  :: S16module      -- contains info about module
  -> S16module      -- updated with result of assembly
assemble x =
  case s16modAsmSrc x of
    Nothing -> x {s16modMetadata = Nothing, s16modObjCode = Nothing}
    Just sourceText ->
      let (locCtr, symtab1, codelines1) = pass1 (lines sourceText)
          (symtab2, codelines2) = pass2 symtab1 codelines1
          (listing, nerrs, errlocs) =
            buildListing False symtab2 codelines2
          obj = pass3 codelines2
          lineAddrTbl = buildLineAddrTable codelines2
      in x { s16modMetadata =
               Just (AsmMetadata
                     { s16modAsmGoodObj     = nerrs==0
                     , s16modAsmListing     = listing
                     , s16modObjSymTbl      = symtab2
                     , s16modLineAddrTable  = lineAddrTbl
                     , s16modNumBadStmts    = nerrs
                     , s16modErrLocs        = errlocs
                     })
           , s16modObjCode  = Just obj
           }

-- not keeping locCtr or codelines2, are these useful?
--  let sourceLines = lines (s16modAsmSrc x)  Deprecated

-- Old version, keep for now... needed for MainTextUI
{-
asm
  :: [String]          -- source program lines
  -> ( Address16       -- final location counter
     , SymbolTable     -- final symbol table
     , [CodeLine]      -- full info about each source line
     , [String]        -- assembly listing
     , Int             -- number of errors
     , [(Int,Int)]     -- error locations
     , LineAddrTable   -- mapping from addresses to souorce lines
     , String          -- object module
     )
asm xs =
  let (locCtr, symtab1, codelines1) = pass1 xs
      (symtab2, codelines2) = pass2 symtab1 codelines1
      (listing, nerrs, errlocs) = buildListing False symtab2 codelines2
      obj = pass3 codelines2
      lineAddrTbl = buildLineAddrTable codelines2
  in (locCtr, symtab2, codelines2, listing,
      nerrs, errlocs, lineAddrTbl, obj)
-}

-- let asmSrcText :: String be the source code.
-- let srclines = lines asmSrcText
-- let (loc, symtab, codelines, listing,
--      nerrs, errlocs, lat, obj) = asm srclines

-- The results can be saved in an AsmModule:

--     case nerrs == 0 of
--       True -> do  -- successful assembly
--         let asmMod =
--               AsmModule {modName = Nothing, filePath = Nothing,
--                          asmSrc = srclines,
--                          objSymTbl = symtab,
--                          asmListing = listing,
--                          asmGoodObj = nerrs == 0,
--                          nBadStmts = nerrs,
--                          errLocs = errlocs,
--                          objCode = obj,
--                          lineAddrTable = lat}

-- asmListing is the human readable source listing
-- highlightErrors can use errLocs to identify error messages
-- obj is the object code


----------------------------------------------------------------------
-- Representation of assembly source module
----------------------------------------------------------------------

----------------------------------------------------------------------

showCodeLine :: CodeLine -> [String]
showCodeLine x =
  ["  Line " ++ show (lineno x) ++ "  address = "
       ++ show (addr x) ++ "\n"
  , "     Source code: <<" ++ src x ++ ">>\n"
  , "     Field1: <<" ++ lineField1 x ++ ">>\n"
  , "        Label: " ++ show (labFld x) ++ "\n"
  , "     Field2: <<" ++ lineField2 x ++ ">>\n"
  , "        Operation: " ++ show (operFld x) ++ "\n"
  , "        Properties: " ++ show (props x) ++ "\n"
  , "     Field3: <<" ++ lineField3 x ++ ">>\n"
  , "     Object code:\n"
  , "     " ++ concat (map ((' ':) . showObjStmt) (codeStmts x))
            ++ "\n"
  ]
     ++ case errs x of
          [] -> [] --   ["     No errors detected"]
          ys -> ["     Error messages: "]
                  ++ map ("       *** " ++) ys

showCodeLines :: [CodeLine] -> String
showCodeLines xs = concat (map (concat . showCodeLine) xs)



----------------------------------------------------------------------
-- Object modules
----------------------------------------------------------------------

data LoadModuleEntry
  = LoadModuleOrg Word16
  | LoadModuleData Word16
  | LoadModuleRel Word16
  deriving Show

type LoadModule = [LoadModuleEntry]


data ObjStmt
  = ObjModule String          -- module statement
  | ObjData [ObjectWord]           -- code or data
  | ObjImport String           -- external name to be given value
  | ObjExport ObjectWord String
  | ObjOrg Word16


showObjStmt :: ObjStmt -> String
showObjStmt (ObjModule xs) = "module " ++ xs ++ "\n"
showObjStmt (ObjData xs) =
--  "data" ++ concat (map ((' ':) . showObjectWord) xs) ++ "\n"
  concat (map ((' ':) . showObjectWord) xs) ++ "\n"
showObjStmt (ObjImport symbol) = "import " ++ symbol ++ "\n"
showObjStmt (ObjExport w symbol) =
  "export " ++ showObjectWord w ++ " " ++ symbol ++ "\n"
showObjStmt (ObjOrg a) = "org " ++ (w16tohex a) ++ "\n"


----------------------------------------------------------------------
-- Representation of assembly source module
----------------------------------------------------------------------

-- The CodeLine data structure holds the components of a source
-- program line.  The parser is called separately on the various
-- fields.

data CodeLine = CodeLine
  { lineno     :: Int                   -- source line number
  , addr       :: Address16             -- address of code generated
  , src        :: String                -- source text
  , lineField1 :: String
  , lineField2 :: String
  , lineField3 :: String
  , labFld     :: LabelField            -- label
  , operFld    :: OperationField        -- mnemonic
  , props      :: Maybe InstrProperties
  , codeStmts  :: [ObjStmt]             -- object code
  , errs       :: [String]              -- error messages
  }


----------------------------------------------------------------------
-- Symbol table
----------------------------------------------------------------------

-- The symbol table is a list of definitions of individual symbols.
-- Information about each symbol is discovered incrementally:

-- * In Pass 1, each label is entered into the symbol table along with
--   its value, which is the location counter.  The expression for
--   labels is *.  This gives a value (which is relocatable) to each
--   label apart from two cases: names defined by an equ statement,
--   and external names (which are given the fixed value 0).

-- * In Pass 2, expressions are evaluated, including expressions in
--   code (addresses and data) and expressions used in equ statements.
--   The expression on the right hand side of an equ statement may use
--   internal labels and names defined by an equ that appears earlier
--   in the source module, ensuring that the names can all be resolved
--   in this one pass.  Expressions may use names defined as labels or
--   equ statements, but not external names.


-- updateSymDef examines a source code line, and if it finds a new
-- name definition this is added to the symbol table.  There are
-- several cases: a label, an equ, and an import.  The strategy is to
-- check first the symbolic operation, because assembler directives
-- treat the label and operand fields in different ways.  If there is
-- a label, and it's an import the value is set to 0; if an equ it is
-- evaluated; otherwise the value is the location counter.  If there
-- is no label, the symbol table is returned unchanged.

updateSymDef
  :: Int             -- source line number
  -> Address16       -- location counter
  -> LabelField      -- label (if any) of code line
  -> OpSymbol        -- the operation
  -> String          -- operand field
  -> SymbolTable     -- current symbol table
  -> ([String],      -- error messages
      SymbolTable)   -- updated symbol table

-- Import: The operand field should be a name, and the symbol is
-- defined with provisional value 0.

updateSymDef n addr labfld Import opand t =
  case labfld of
    LabelAbsent -> (["no label on import"], t)
    LabelBadSyntax msgs -> (msgs, t)
    Label xs ->
      ([], insertSymbol n addr xs SymImport ExpStar 0 t)

{- Export: the label field should be empty, and no symbol is
defined. -}

updateSymDef n addr labfld Export opand symtab =
  case labfld of
    LabelAbsent -> ([], symtab)
    LabelBadSyntax msgs ->
      ("Export should not have label" : msgs, symtab)
    Label xs ->
      (["Export should not have label"], symtab)

{- Equ: the symbol is defined to be the value of the expression. -}

updateSymDef n addr labfld EquStmt opand t =
  case (parse expr "" opand) of
    Left err -> (lines (show err), t)
    Right exp ->
      let (msgs,k) = evalMsgs addr t exp
      in case labfld of
           LabelAbsent -> (["missing label on equ"], t)
           LabelBadSyntax lmsgs -> (lmsgs, t)
           Label xs ->
             ([], insertSymbol n addr xs SymEqu exp (objectWordVal k) t)

-- For all other cases, including all the instructions, if there is a
-- label then the symbol value is the location counter.  If there is
-- no label the symbol table is returned unchanged.

updateSymDef n addr labfld anyop opand t =
  case labfld of
    LabelAbsent -> ([], t)
    LabelBadSyntax msgs -> (msgs, t)
    Label xs -> ([], insertSymbol n addr xs SymLabel ExpStar addr t)

-- Insert a new symbol into the symbol table.  If the symbol is
-- already there, its properties aren't changed, and an additional
-- deflines entry is made.  Each symbol should be defined in only one
-- place, and if deflines has length 2 or more this will trigger an
-- error message later on.

insertSymbol
  :: Int          -- source line number
  -> Address16    -- location counter
  -> String       -- symbol name
  -> SymWhere     -- where it's defined
  -> Expr         -- expression for the symbol
  -> Word16       -- value of the symbol
  -> SymbolTable   -- current symbol table
  -> SymbolTable   -- return the updated symbol table

insertSymbol n a sym w exp val [] =
  [SymbolDef {symName=sym, symWhere=w, symExpr=exp,
              symVal=val, defLines=[n], useLines=[]}]
insertSymbol n a sym w exp val (x:xs) =
  if symName x == sym
    then  x {defLines = n : defLines x} : xs
    else x :  insertSymbol n a sym w exp val xs


{- The following functions convert the symbol table to formatted
strings testing and for the assembly listing. -}

showSymTab :: SymbolTable -> [String]
showSymTab xs =
   "Symbol    Value Where Def  Used\n\n"
   : map showSymTabEntry xs

showSymTabEntry :: SymbolDef -> String
showSymTabEntry x =
    fillString 10 (symName x)
    ++ inthexok 4 (symVal x) ++ "   "
    ++ showSymWhere (symWhere x)
    ++ "  " ++ show (defLines x)
    ++ "   " ++ show (useLines x)
    ++ "\n"

printSymTab :: SymbolTable -> IO ()
printSymTab [] =  return ()
printSymTab (x:xs) =
  do putStrLn ("  " ++ show x)
     printSymTab xs

----------------------------------------------------------------------
-- Source line address table
----------------------------------------------------------------------

-- Build table for processor pane to highlight source code line
-- corresponding to an address.

buildLineAddrTable ::[CodeLine] -> [(Word16,Int)]
buildLineAddrTable [] = []
buildLineAddrTable (x:xs) =
  case codeStmts x of
    [] -> buildLineAddrTable xs
    (w:ws) -> (addr x, lineno x) : buildLineAddrTable xs

lookupLineAddr :: Word16 -> [(Word16,Int)] -> Int
lookupLineAddr a [] = 0
lookupLineAddr a ((x,n):xs) =
  if x==a
    then n
    else lookupLineAddr a xs

----------------------------------------------------------------------
-- Assembler Pass 1
----------------------------------------------------------------------

-- Pass 1 parses the source code, uses the mnemonic field to decide
-- what kind of statement each line is, maintains the location
-- counter, and defines the labels.

pass1 :: [String] -> (Address16, SymbolTable, [CodeLine])
pass1 = pass1loop 1 0 []

pass1loop
  :: Int -> Address16 -> SymbolTable -> [String]
  -> (Address16, SymbolTable, [CodeLine])
pass1loop n a t [] = (a,t,[])

pass1loop n a t (x:xs) =
  let (a',t',cl) = pass1step n a t x
      (a'',t'',cls) = pass1loop (n+1) a' t' xs
  in (a'',t'',cl:cls)

pass1step
  :: Int            -- source line number
  -> Address16      -- location counter
  -> SymbolTable    -- current environment
  -> String         -- text of source line
  -> (Address16,    -- updated location counter
      SymbolTable,  -- updated symbol table
      CodeLine)     -- info about this source line

pass1step n a t s = (a + fromIntegral size, t', cline)
  where
    (errs0,(field1,field2,field3,field4)) = buildFields s
    lf = buildLabelField field1
    (errs2,opf) = buildOperationField field2
    iprops = lookupMnemonic opf instrProperties
    opsym = case iprops of
              Nothing -> EmptyStmt
              Just (mne,op,fmt,opcode) -> op
    size = case iprops of
             Nothing -> 0
             Just ps -> fmtSize (propFmt ps)
    (errs3, t') = updateSymDef n a lf opsym field3 t
    cline = CodeLine
              {lineno = n, addr = a, src = s,
               lineField1 = field1, lineField2 = field2,
               lineField3 = field3,
               labFld = lf, operFld = opf,
               props = iprops, codeStmts = [],
               errs = errs0++errs2++errs3}

----------------------------------------------------------------------
-- Fields of an assembly statement
----------------------------------------------------------------------

data LabelField
  = Label String
  | LabelBadSyntax [String]
  | LabelAbsent
  deriving Show

data OperationField
  = Mnemonic String
  | MnemonicBadSyntax
  | NoMnemonic
  deriving Show

data Const_operand_src
  = Constop_syntax_ok Expr
  | Constop_syntax_err [String]
  deriving Show

data RRR_operand_src
  = RRR_syntax_ok RegisterSrc RegisterSrc RegisterSrc
  | RRR_syntax_err [String]
  deriving Show

data RR_operand_src
  = RR_syntax_ok RegisterSrc RegisterSrc
  | RR_syntax_err [String]
  deriving Show

data EXP_operand_src
  = EXP_syntax_ok RegisterSrc RegisterSrc RegisterSrc
  | EXP_syntax_err [String]
  deriving Show

data RRRK_operand_src
  = RRRK_syntax_ok RegisterSrc RegisterSrc RegisterSrc Expr
  | RRRK_syntax_err [String]
  deriving Show

data RRRRK_operand_src
  = RRRRK_syntax_ok RegisterSrc RegisterSrc RegisterSrc
      RegisterSrc Expr
  | RRRRK_syntax_err [String]
  deriving Show

data RRRRR_operand_src
  = RRRRR_syntax_ok RegisterSrc RegisterSrc RegisterSrc
       RegisterSrc RegisterSrc
  | RRRRR_syntax_err [String]
  deriving Show

data RX_operand_src
  = RX_syntax_ok RegisterSrc (Expr, RegisterSrc)
  | RX_syntax_err [String]
  deriving Show

data JX_operand_src
  = JX_syntax_ok (Expr, RegisterSrc)
  | JX_syntax_err [String]
  deriving Show

{- deprecated
data X_operand_src
  = X_syntax_ok (Expr, RegisterSrc)
  | X_syntax_err [String]
-}

----------------------------------------------------------------------
-- Assembler Pass 2
----------------------------------------------------------------------

-- Pass2 evaluates expressions and generates the code

pass2 :: SymbolTable -> [CodeLine] -> (SymbolTable, [CodeLine])
pass2 t [] = (t,[])
pass2 t (x:xs) =
  let (t',x') = pass2step t x
      (t'',xs') = pass2 t' xs
  in (t'', x':xs')

pass2step :: SymbolTable -> CodeLine -> (SymbolTable, CodeLine)
pass2step t x = (t',x')
  where
    n = lineno x
--    su = symbolused (opandFld x)
    ems = errs x
    dupLabMsgs = checkForDuplicateLabelDef t x
    t' = t -- update with symbol used, for printing symbol table
    (ms,objcode) = codegen t x
    x' = x {errs=dupLabMsgs++ems++ms, codeStmts=objcode}

-- generate error message if label has multiple definitions

checkForDuplicateLabelDef :: SymbolTable -> CodeLine -> [String]
checkForDuplicateLabelDef t c =
  case labFld c of
    LabelBadSyntax _ -> []
    LabelAbsent -> []
    Label xs ->
      case lookupSym xs t of
        Nothing -> [] -- this case is impossible
        Just sdef ->
          if length (defLines sdef) >1
            then ["There are multiple definitions of label " ++ xs,
                  "See symbol table for list of all definitions"]
            else []


----------------------------------------------------------------------
-- Locate the fields of an assembly language statement
----------------------------------------------------------------------

-- getFields splits a line into four fields, separated by white space.
-- It should never fail; if fewer than four fields exist, the later
-- fields will be empty strings, and if the entire line is empty, then
-- all four fields will be empty too.

getFields :: Parser (String,String,String,String)
getFields =
  do f1 <- nonWhite
     skipWhiteSpace
     f2 <- nonWhite
     skipWhiteSpace
     f3 <- nonWhite
     f4 <- restOfLine
     return (f1,f2,f3,f4)

buildFields
  :: String
  -> ([String], (String,String,String,String))

buildFields s =
  case (parse getFields "" s) of
    Left err -> (["Cannot extract fields"],
                 ("","","",""))
    Right fs -> ([],fs)

----------------------------------------------------------------------
-- Parse the label field
----------------------------------------------------------------------

labelErrs :: LabelField -> [String]
labelErrs (Label _) = []
labelErrs (LabelBadSyntax xs) = xs
labelErrs LabelAbsent = []

labelStr :: LabelField -> String
labelStr (Label xs) = xs
labelStr (LabelBadSyntax _) = ""
labelStr LabelAbsent = ""

labelPresent :: Parser LabelField
labelPresent =
  do x <- name
     eof
     return (Label x)

labelAbsent :: Parser LabelField
labelAbsent =
  do skipWhiteSpace
     eof
     return LabelAbsent

labelField :: Parser LabelField
labelField = try labelPresent <|> labelAbsent

buildLabelField :: String -> LabelField
buildLabelField s =
  case (parse labelAbsent "" s) of
    Left erra ->
      case (parse labelPresent "" s) of
        Left errp ->
          let xs = lines (show errp)
              ys = "Incorrect syntax in label field:"
                     : map ("       "++) xs
          in LabelBadSyntax ys
        Right x -> x
    Right x -> x

----------------------------------------------------------------------
-- Parse the operation field
----------------------------------------------------------------------

operationPresent :: Parser OperationField
operationPresent =
  do x <- name
     eof
     return (Mnemonic x)

operationAbsent :: Parser OperationField
operationAbsent =
  do skipWhiteSpace
     eof
     return NoMnemonic

operationField :: Parser OperationField
operationField = try operationAbsent <|> operationPresent

buildOperationField :: String -> ([String],OperationField)
buildOperationField s =
  case (parse operationAbsent "" s) of
    Left erra ->
      case (parse operationPresent "" s) of
        Left err ->
          let xs = lines (show err)
              ys = "Incorrect syntax in operation field:"
                     : map ("       "++) xs

          in (ys, MnemonicBadSyntax)
        Right x -> ([], x)
    Right x -> ([], x)

----------------------------------------------------------------------
-- Parse the operand field, seeking constant operand
----------------------------------------------------------------------

-- A constant operand, used in data statement

build_Constop :: String -> Const_operand_src
build_Constop s =
  case (parse expr "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Syntax error in constant field: " ++) xs
      in Constop_syntax_err ys
    Right x -> Constop_syntax_ok x

----------------------------------------------------------------------
-- Parse the operand field, seeking RRR operand
----------------------------------------------------------------------

-- An RRR operand is represented as an RRR_operand_src object.  If the
-- parser succeeds, it returns this value. Even if the parse succeeds,
-- the register fields might be invalid, e.g. a number like 17 that is
-- syntactically ok but doesn't refer to an actual register.  If the
-- parse fails, the error messages are recorded with the
-- RRR_syntax_err constructor.  In this case, the operand field is not
-- syntactically valid as RRR; it might be some other kind of operand
-- field, or gibberish, or even empty.


-- build_RRR: take the operand field, which is a string, and try to
-- interpret it as an RRR operand, recording the result as an
-- RRR_operand_src.

build_RRR :: String -> RRR_operand_src
build_RRR s =
  case (parse rrrField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax in RRR operand field: " ++)
                   xs
      in RRR_syntax_err ys
    Right x -> x

-- rrrField: parse a string according to the RRR syntax rules; either
-- succeed with the three registers, encapsulated in an
-- RRR_operand_src object, or fail.

rrrField :: Parser RRR_operand_src
rrrField =
  do r1 <- register
     char ','
     r2 <- register
     char ','
     r3 <- register
     eof
     return (RRR_syntax_ok r1 r2 r3)

----------------------------------------------------------------------
-- Parse the operand field, seeking RRRK operand
----------------------------------------------------------------------

-- build_RRRK: take the operand field, which is a string, and try to
-- interpret it as an RRR operand, recording the result as an
-- RRR_operand_src.

build_RRRK :: String -> RRRK_operand_src
build_RRRK s =
  case (parse rrrkField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax in RRRK operand: " ++)
                   xs
      in RRRK_syntax_err ys
    Right x -> x

-- Parse a string according to the RRRK syntax rules

rrrkField :: Parser RRRK_operand_src
rrrkField =
  do r1 <- register
     char ','
     r2 <- register
     char ','
     r3 <- register
     k <- register  -- should really be hex2
     eof
     return undefined -- (RRRK_syntax_ok r1 r2 r3 k)

----------------------------------------------------------------------
-- Parse the operand field, seeking RR operand
----------------------------------------------------------------------

-- This is similar to parsing of an RRR operand.  The only difference
-- is that the operand consists of two registers, sa and sb.


{- build_RR: take the operand field, which is a string, and try to
interpret it as an RR operand, recording the result as an
RR_operand_src. -}

build_RR :: String -> RR_operand_src
build_RR s =
  case (parse rrField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax in RR operand field: " ++)
                   xs
      in RR_syntax_err ys
    Right x -> x

-- rrField: parse a string according to the RR syntax rules; either
-- succeed with the two registers, encapsulated in an RR_operand_src
-- object, or fail.

rrField :: Parser RR_operand_src
rrField =
  do r2 <- register
     char ','
     r3 <- register
     eof
     return (RR_syntax_ok r2 r3)

----------------------------------------------------------------------
-- Parse the operand fields, seeking RX operand
----------------------------------------------------------------------

build_RX :: String -> RX_operand_src
build_RX s =
  case (parse rxField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax in RX operand: " ++) xs
      in RX_syntax_err ys
    Right x -> x

rxField :: Parser RX_operand_src
rxField =
  do r <- register
     char ','
     a <- dispBase
     return (RX_syntax_ok r a)

dispBase :: Parser (Expr, RegisterSrc)
dispBase =
  do disp <- expr
     char '['
     r <- register
     char ']'
     return (disp,r)

----------------------------------------------------------------------
-- Parse the operand fields, seeking JX operand
----------------------------------------------------------------------

-- JX is a pseudoformat.  In assembly language there is just one
-- component of the operand, an address: no destination register is
-- specified.  JX is used for jump instructions, and the machine
-- language representation is RX (the dst field is used to specify the
-- condition for the jump).  The parsing of JX is similar to that of
-- RX, but the destination register is omitted.


build_JX :: String -> JX_operand_src
build_JX s =
  case (parse jxField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax in JX operand: " ++) xs
      in JX_syntax_err ys
    Right x -> x

jxField :: Parser JX_operand_src
jxField =
  do a <- dispBase
     return (JX_syntax_ok a)

----------------------------------------------------------------------
-- Parse operand field, seeking X operand
----------------------------------------------------------------------

{- deprecated
build_X :: String -> X_operand_src
build_X s =
  case (parse xField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax error in X operand field: " ++)
                   xs
      in X_syntax_err ys
    Right x -> x

xField :: Parser X_operand_src
xField =
  do a <- dispBase
     return (X_syntax_ok a)
-}

----------------------------------------------------------------------
-- Generate object code
----------------------------------------------------------------------

codegen
  :: SymbolTable     -- environment
  -> CodeLine        -- info about source code line
  -> ([String],      -- error messages
      [ObjStmt])  -- object code

codegen t x =
  let op = operFld x
      lab = labelStr (labFld x)
      codeaddr = addr x
      operand = lineField3 x
      labelErrMsgs = labelErrs (labFld x)
  in case props x of
       Nothing ->
         if lineField2 x == ""
           then (labelErrMsgs, []) -- empty statement
           else (labelErrMsgs ++
                   ["Invalid mnemonic: " ++ lineField2 x], [])
       Just iprops ->
         case propFmt iprops of
           Empty  -> (labelErrMsgs, [])
           AsmDir ->
             case propOp iprops of
               AsmMod -> (labelErrMsgs, [ObjModule lab])
               Export ->
                 case lookupSym operand t of
                   Just symdef ->
                     ([], [ObjExport (ObjFixed (symVal symdef))
                             operand])
                   Nothing -> (["unknown symbol"],[])
               Import -> ([], [ObjImport lab])
               EquStmt -> ([],[])
               OrgStmt -> ([],[])
               _ -> ([],[])
           Con    -> genCon codeaddr t labelErrMsgs
                       (build_Constop operand)
           RRR    -> genRRR labelErrMsgs iprops (build_RRR operand)
           RR     -> genRR labelErrMsgs iprops (build_RR operand)
           RRRK   -> genRRRK labelErrMsgs iprops (build_RRRK operand)
           RX     -> genRX codeaddr t labelErrMsgs iprops
                       (build_RX operand)
           JX     -> genJX codeaddr t labelErrMsgs iprops
                       (build_JX operand)
           EXP    -> genEXP labelErrMsgs t iprops (build_RRR operand)

genRRR
  :: [String] -> InstrProperties -> RRR_operand_src
  -> ([String], [ObjStmt])

genRRR laberrs (mne,op,fmt,opcode) opand =
  let opcodedig = opcode!!0
  in case opand of
       RRR_syntax_ok rd ra rb ->
         let (m1,d1) = regtoInt rd
             (m2,d2) = regtoInt ra
             (m3,d3) = regtoInt rb
             code = ObjFixed (buildword (opcodedig,d1,d2,d3))
         in (laberrs++m1++m2++m3, [ObjData [code]])
       RRR_syntax_err es -> (laberrs++es, [ObjData [obj0]])

genRRRK
  :: [String] -> InstrProperties -> RRRK_operand_src
  -> ([String], [ObjStmt])

genRRRK laberrs (mne,op,fmt,opcode) opand = undefined
{- ?????????????
  let opcodedig0 = opcode!!0
      opcodedig1 = opcode!!1
  in case opand of
       RRR_syntax_ok rd ra rb i ->
         let (m1,d1) = regtoInt rd
             (m2,d2) = regtoInt ra
             (m3,d3) = regtoInt rb
             (m4,k)  = evalCon8 i 
             w1 = ObjFixed (buildword (opcodedig0,d1,d2,d3))
             w2 = ObjFixed (
         in (laberrs++m1++m2++m3, [ObjData [w1,w2]])
       RRR_syntax_err es -> (laberrs++es, [ObjData [obj0]])
-}

-- The cmp instruction is classified as RR, because in assembly
-- language only two register are specified: cmp R2,R9.  It generates
-- an RRR instruction, with the dst field a don't care value of 0.  RR
-- is a pseudo format: it exists in assembly language but not in
-- machine language.

genRR
  :: [String] -> InstrProperties -> RR_operand_src
  -> ([String], [ObjStmt])

genRR laberrs (mne,op,fmt,opcode) opand =
  let opcodedig = opcode!!0
  in case opand of
       RR_syntax_ok ra rb ->
         let (m2,d2) = regtoInt ra
             (m3,d3) = regtoInt rb
             code = ObjFixed (buildword (opcodedig,0,d2,d3))
         in (laberrs++m2++m3, [ObjData [code]])
       RR_syntax_err es -> (laberrs++es, [ObjData [obj0]])

genRX
  :: Address16                   -- address where this will go
  -> SymbolTable                 -- environment
  -> [String]                    -- laberrs
  -> InstrProperties             -- info about the instructino
  -> RX_operand_src              -- operand
  -> ([String], [ObjStmt])    -- (errors, code)

genRX a t laberrs (mne,op,fmt,opcode) opand =
  let opcodedig0 = opcode!!0
      opcodedig1 = opcode!!1
  in case opand of
       RX_syntax_ok rd (v,rx) ->
         let (m1,d1) = regtoInt rd
             (m2,dx) = regtoInt rx
             (m3,k) = evalMsgs a t v
             c1 = ObjFixed (buildword (opcodedig0,d1,dx,opcodedig1))
         in (laberrs++m1++m2++m3, [ObjData [c1,k]])
       RX_syntax_err es -> (laberrs++es, [ObjData [obj0,obj0]])


genJX
  :: Address16                   -- address where this will go
  -> SymbolTable                 -- environment
  -> [String]                    -- laberrs
  -> InstrProperties             -- info about the instructino
  -> JX_operand_src              -- operand
  -> ([String], [ObjStmt])    -- (errors, code)

genJX a t laberrs (mne,op,fmt,opcode) opand =
  let opcodedig0 = opcode!!0
      opcodedig1 = opcode!!1
      opcodedig2 = opcode!!2
  in case opand of
       JX_syntax_ok (v,rx) ->
         let (m2,dx) = regtoInt rx
             (m3,k) = evalMsgs a t v
             c1 = ObjFixed (buildword
                    (opcodedig0,opcodedig2,dx,opcodedig1))
         in (laberrs++m2++m3, [ObjData [c1,k]])
       JX_syntax_err es -> (laberrs++es, [ObjData [obj0,obj0]])


genEXP :: [String] -> SymbolTable -> InstrProperties -> RRR_operand_src
         -> ([String], [ObjStmt])
genEXP laberrs t (mne,op,fmt,opcode) opand =
  let opcodedig0 = hexDigits!!(opcode!!0)
  in case opand of
       RRR_syntax_ok rd ra rb ->
         let (m1,d1) = regtoInt rd
             (m2,d2) = regtoInt ra
             (m3,d3) = regtoInt rb
             (m4, maybe_op8) = inthex 2 (opcode!!1)
             op8 = case maybe_op8 of
                     Just x -> x
                     Nothing -> "00"
             code1 = obj0
             code2 = obj0
         in (laberrs++m1++m2++m3++m4, [ObjData [code1,code2]])
       RRR_syntax_err es -> (laberrs++es, [ObjData [obj0, obj0]])


-- regtoInt replaces reg_hex; it returns the register number with a
-- (possible) error message reg_hex takes a source specifcation of a
-- register and produces a hex character to be used in generated
-- code.

regtoInt :: RegisterSrc -> ([String], Int)
regtoInt (RegHex x) = ([], hexval x)
regtoInt (RegDec xs) =
  let n = (read xs) :: Int
  in if 0 <= n && n < 16
       then ([], n)
       else (["Invalid register number: " ++ show n], 0)

{- Generate an instruction word that is a constant (Con format).  This
is used by the data statement. -}

genCon
  :: Address16 -> SymbolTable -> [String] -> Const_operand_src
  -> ([String], [ObjStmt])
genCon a t laberrs x =
  case x of
    Constop_syntax_ok exp ->
        let (evalErrs, v) = evalMsgs a t exp
        in (laberrs++evalErrs, [ObjData [v]])
    Constop_syntax_err xs -> (laberrs++xs, [ObjData [obj0]])


sym_code :: SymbolTable -> String -> ([String], Maybe Hex4)
sym_code t s =
  case lookupSym s t of
    Just x -> word16_hex4 (fromIntegral (symVal x))
    Nothing -> (["Undefined symbol: " ++ s], Nothing)

----------------------------------------------------------------------

-- con_code :: Constant -> ([String], [ObjectWord])

con_code :: Expr -> ([String], [ObjectWord])
con_code exp = error "not done"

{-
con_code (ConstantI i) = inthex 4 i
con_code (ConstantHex s) =
  if valid_hex s && length s == 4
    then ([], Just s)
    else (["Invalid 4-digit hex constant: " ++ s], Nothing)
con_code (ConstantBits s) = error "con_code"
-}

valid_hex :: String -> Bool
valid_hex xs = all (flip elem hexDigits) (map toLower xs)

-- symbolused takes an operand, and returns a symbol used in the
-- operand (if any).  Its purpose is to help generate the symbol
-- table.  Temporarily disabling this; several symbols could appear in
-- an espression so this should return a list, not a Maybe.  ?????

symbolused :: OperandField -> Maybe String
symbolused _ = Nothing -- temporary disabling
{-
symbolused (Operand_k _) = Nothing
symbolused (Operand_RRR _ _ _) = Nothing
symbolused (Operand_RX _ (ValCon _, _)) = Nothing
symbolused (Operand_RX _ (ValSym x, _)) = Just x
symbolused NoOperand = Nothing
symbolused OperandBadSyntax = Nothing
-}

----------------------------------------------------------------------
-- Representation of assembly object module
----------------------------------------------------------------------

type ObjectFileBaseName = Maybe String

-- The information in each source statement is represented as an
-- ObjStmt.  This could be in instruction, but also includes metadata.
-- Object code words are shown as either F.01ab (for fixed data) or
-- R.9876 (for relocatable data).

-- Here is an example of a very simple object module (the values are
-- random), consisting simply of a sequence of word constants.  There
-- are no external references, so this object module does not need to
-- be linked; it can be loaded directly by the bootstrap loader:

--    02b1 0de3
--    ff30 1234 c27d

-- An object module may include several other items:

--    module OrbitalCalculation
--    org 00f0
--    bc32 893d f023
--    1234 abcd
--    import bin2string
--    0283
--    export "subr1"
--    f923 00c8

-- The keywords are:

--  * module is followed by a symbol which names the module

--  * org is followed by address; subsequent data will be loaded
--    into memory from that address.  By default, the initial
--    org is 0, so if there is no org statement the module will be
--    loaded into memory starting from location 0.

--  * import is followed by a symbol which must be exported by some
--    other module known to the linker.  The linker will resolve the
--    value of the symbol, which must be a word.  The value of this
--    word is loaded into memory.  For example, suppose the external
--    name bin2string has the value 2cd9.  Then "1234 import
--    bin2string abcd" is equivalent to "1234 2cd9 abcd".

--  * export is followed by a symbol.  The linker will give this
--   symbol the value of the current location counter, after
--   relocation, and that value may be imported by other modules.

--  * relocate is followed by a constant x.  The linker will replace
--    the value by x+a, where a is the relocation offset for this
--    module.

----------------------------------------------------------------------
-- Pass 3:  create object module
----------------------------------------------------------------------

-- Pass3 traverses the CodeLines and produces the object code module,
-- which can be read by the linker or loader.


pass3 :: [CodeLine] -> String
pass3 xs =
  concat (map showObjStmt (concat (map buildObjStmt xs)))

buildObjStmt :: CodeLine -> [ObjStmt]
buildObjStmt cl =
  case codeStmts cl of
    [] -> []
    xs -> codeStmts cl

{-
buildObjectCodeLine x =
  let code =
        "data "
        ++ inthexok 4 (addr x)
        ++ "  "
        ++ showTwoCodeWords (codeStmts x)
        ++ "\n"
  in code
-}

----------------------------------------------------------------------
-- Format output from assembler
----------------------------------------------------------------------

putSymTab :: SymbolTable -> IO ()
putSymTab st =
  do putStrLn "Symbol table:"
     putStrLn (concat (map ((++"\n") . show) st))


asmListingHeader :: String
asmListingHeader =
  "Line  Addr  Code       Source statement\n"

asmListingLine :: Bool -> CodeLine -> [String]
asmListingLine verbose x =
  let mainListingLine =
        fmtInt 4 (lineno x)
        ++ "  "
        ++ inthexok 4 (addr x)
        ++ "  "
        ++ showTwoCodeWords (codeStmts x)
        ++ "  "
        ++ src x
        ++ "\n"
      internalCodeRep =
        if verbose
          then "\nInternal representation:" : showCodeLine x ++ ["\n\n"]
          else []
      errorlines =
        case errs x of
          [] -> []
          xs -> let f msg = "*** Error.  " ++ msg ++ "\n"
                in map f xs ++ ["\n"]
  in mainListingLine : internalCodeRep ++ errorlines

-- Above, ?????   Try to use markup to make messages red...
--          _ -> "<span foreground=\"red\">\n" :
--                  errs x ++ ["</span>\n\n\n"]

extractCode :: [CodeLine] -> [ObjStmt]
extractCode = concat . map codeStmts

-- The assembly listing shows the object code for each instruction.
-- To keep the listing aligned in columns, each listing line should
-- use 9 characters, allowing for two code words, even if there is
-- only one code word or none at all.  If there's one code word the
-- second is shown as blank, and if the line doesn't contain an
-- instruction then both are shown as blank.  If there are more than
-- two code words (e.g. for a string constant in a data statement)
-- they are all shown, and the alignment will be off for just that
-- statement.

showTwoCodeWords :: [ObjStmt] -> String
showTwoCodeWords [] = "         "
showTwoCodeWords (stmt:stmts) =
  case stmt of
    ObjData []    -> "         "
    ObjData [x]   -> showObjectWordData x ++ "     "
    ObjData [x,y] -> showObjectWordData x ++ " " ++ showObjectWordData y
    ObjData xs    -> concat (map showObjectWordData xs)

----------------------------------------------------------------------
-- Generate assembler listing
----------------------------------------------------------------------

-- Build a listing, along with a count of the number of lines that
-- produced error messages, and a table showing the line numbers in
-- listing that contain error messages.

buildListing
  :: Bool -> SymbolTable -> [CodeLine]
  -> ([String], Int, [(Int,Int)])

buildListing verbose t xs =
  let (ys, nbad, locs) = buildListingLoop verbose t xs 0 [] 0 []
      errorReport =
        if nbad == 0
          then [] -- "No errors detected\n"
          else ["Errors were found in "
                   ++ show nbad
                   ++ (if nbad>1 then " lines\n" else " line\n"),
                "\n"]
      zs = (errorReport ++ [asmListingHeader, "\n"]) : reverse ys
  in (concat zs ++ ["\n","\n"] ++ showSymTab t, nbad, locs)

buildListingLoop verbose t [] k listing nbadstmts errlinelocs =
  (listing, nbadstmts, errlinelocs)

buildListingLoop verbose t (x:xs) k listing nbadstmts errlinelocs =
  case asmListingLine verbose x of
    [y] -> -- no error here
      buildListingLoop verbose t xs (k+1)
        ([y] : listing) nbadstmts errlinelocs
    ys -> -- have error messages
      buildListingLoop verbose t xs (k + length ys)
        (ys:listing) (nbadstmts+1)
        ((k+1, k + length ys - 1) : errlinelocs)

----------------------------------------------------------------------
operandField = undefined
{-
operandField :: Parser OperandField
operandField =
  do skipWhiteSpace
     try operand_RRR <|> operand_RX <|> operand_k
-}


buildOperandField :: String -> ([String],OperandField)
buildOperandField s =
  case (parse operandField "" s) of
    Left err ->
      let xs = lines (show err)
          ys = map (" Incorrect syntax error in operand field: " ++)
                   (tail xs)
      in (ys, OperandBadSyntax)
    Right x ->  ([], x)

-- parseStatement
--   :: String
--   -> Parser ([String], LabelField, OperationField, OperandField)

----------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------



----------------------------------------------------------------------
-- Evaluating expressions
----------------------------------------------------------------------


obj0 :: ObjectWord
obj0 = ObjFixed 0

objectWordVal :: ObjectWord -> Word16
objectWordVal (ObjFixed w) = w
objectWordVal (ObjRel w) = w


showObjectWord :: ObjectWord -> Hex4
showObjectWord (ObjFixed x) = w16tohex x
showObjectWord (ObjRel x) =  "relocate " ++ w16tohex x


{- this is from earlier versions, 2.1 and before
showObjectWord :: ObjectWord -> Hex4
showObjectWord (ObjFixed x) = 'F': '.' : w16tohex x
showObjectWord (ObjRel x) = 'R' : '.' : w16tohex x
-}



-- Same as showObjectWord but the F. or R. is omitted; only the actual
-- data value is shown.

showObjectWordData :: ObjectWord -> Hex4
showObjectWordData (ObjFixed x) = w16tohex x
showObjectWordData (ObjRel x) = w16tohex x

-- An expression is evaluated in the context of the symbol table and
-- the current location, which is the value of the * expression.  The
-- result is a word which is marked as fixed or relocatable.  cl is
-- the current location; syt is the symbol table.

-- evalMsgs is an interface to eval; it doesn't use an error monad,
-- but simply returns a list of error messages (which may be empty),
-- and an object code word (which should be 0 if there is an error).

evalMsgs
  :: Address16                 -- cl = current location
  -> SymbolTable               -- syt = environment
  -> Expr                      -- expression to evaluate
  -> ([String], ObjectWord)  -- object code word, or error message
evalMsgs cl syt exp =
  case eval cl syt exp of
    Left m -> ([m], obj0)
    Right w -> ([], w)

eval
  :: Address16                 -- cl = current location
  -> SymbolTable               -- syt = environment
  -> Expr                      -- expression to evaluate
  -> Either String ObjectWord  -- object code word, or error message

eval cl syt ExpStar = Right $ ObjRel cl

eval cl syt (ExpCon k) = Right $ ObjFixed (evalConst k)

eval cl syt (ExpSym xs) =
  case lookupSym xs syt of
    Nothing -> Left $ "undefined symbol: " ++ xs
    Just symDef -> Right $ ObjRel (symVal symDef)

eval cl syt (ExpRel x) =
  do x' <- eval cl syt x
     r <- markRel x'
     return r

eval cl syt (ExpFixed x) =
  do x' <- eval cl syt x
     r <- markFixed x'
     return r

eval cl syt (ExpSum x y) =
  do x' <- eval cl syt x
     y' <- eval cl syt y
     r <- objAdd x' y'
     return r

eval cl syt (ExpDiff x y) =
  do x' <- eval cl syt x
     y' <- eval cl syt y
     r <- objSub x' y'
     return r

eval cl syt (ExpProd x y) =
  do x' <- eval cl syt x
     y' <- eval cl syt y
     r <- objMul x' y'
     return r

-- The following functions do arithmetic on object code words.  The
-- result, and the legality of the operation, depend on whether the
-- code words are fixed or relative.

markRel :: ObjectWord -> Either String ObjectWord
markRel (ObjFixed x) = Right $ ObjRel x
markRel (ObjRel x) = Right $ ObjRel x

markFixed :: ObjectWord -> Either String ObjectWord
markFixed (ObjFixed x) = Right $ ObjFixed x
markFixed (ObjRel x) = Right $ ObjFixed x

objAdd :: ObjectWord -> ObjectWord -> Either String ObjectWord
objAdd (ObjFixed x) (ObjFixed y) = Right $ ObjFixed (x+y)
objAdd (ObjRel x) (ObjFixed y) = Right $ ObjRel (x+y)
objAdd (ObjFixed x) (ObjRel y) = Right $ ObjRel (x+y)
objAdd (ObjRel x) (ObjRel y) = Left "sum of two relative expressions"

objSub :: ObjectWord -> ObjectWord -> Either String ObjectWord
objSub (ObjFixed x) (ObjFixed y) = Right $ ObjFixed (x-y)
objSub (ObjRel x) (ObjFixed y) = Right $ ObjRel (x-y)
objSub (ObjFixed x) (ObjRel y) = Left "subtracting a relocatable"
objSub (ObjRel x) (ObjRel y) = Right $ ObjFixed (x-y)


objMul :: ObjectWord -> ObjectWord -> Either String ObjectWord
objMul (ObjFixed x) (ObjFixed y) = Right $ ObjFixed (x*y)
objMul (ObjRel x) (ObjFixed y) = Left "product of relocatable"
objMul (ObjFixed x) (ObjRel y) = Left "product of relocatable"
objMul (ObjRel x) (ObjRel y) = Left "product of relocatable"

objDiv :: ObjectWord -> ObjectWord -> Either String ObjectWord
objDiv (ObjFixed x) (ObjFixed y) = Right $ ObjFixed (x `div` y)
objDiv (ObjRel x) (ObjFixed y) = Left "division of relocatable"
objDiv (ObjFixed x) (ObjRel y) = Left "division of relocatable"
objDiv (ObjRel x) (ObjRel y) =  Left  "division of relocatable"

----------------------------------------------------------------------
-- Parsing expressions
----------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser exprTable exprFactor

exprTable =
  [ [Prefix mkRel, Prefix mkFixed]
  , [Infix mkProd AssocLeft]
  , [Infix mkSum AssocLeft, Infix mkDiff AssocLeft]
  ]

mkProd  = string "*" >> return ExpProd
mkSum   = string "+" >>  return ExpSum
mkDiff  = string "-" >>  return ExpDiff
mkRel   = string "&" >>  return ExpRel
mkFixed = string "@" >> return ExpFixed

exprFactor = 
  do char '('
     x <- expr
     char ')'
     return x
  <|> expSym
  <|> expCon
  <|> expStar

valSym :: Parser Val16
valSym =
  do s <- name
     return (ValSym s)

expSym :: Parser Expr
expSym =
  do x <- name
     return (ExpSym x)

expCon :: Parser Expr
expCon =
  do x <- constant
     return (ExpCon x)

val16 :: Parser Val16
val16 = valCon <|> valSym

expStar :: Parser Expr
expStar =
  do char '*'
     return ExpStar

----------------------------------------------------------------------
-- Parse constants
----------------------------------------------------------------------

constant :: Parser Constant
constant =
  try constantHex <|>
    try constantBits <|>
    constantNegI <|>
    constantI

valCon :: Parser Val16
valCon =
  do c <- constant
     return (ValCon c)

constantNegI :: Parser Constant
constantNegI =
  do char '-'
     n <- asmNatural
     return (ConstantI (- (fromIntegral n)))

constantI :: Parser Constant
constantI =
  do n <- asmNatural
     return (ConstantI (fromIntegral n))

constantHex :: Parser Constant
constantHex =
  do -- char '0'
     -- char 'x'
     char '$'
     s <- hexString
     return (ConstantHex s)

constantBits :: Parser Constant
constantBits =
  do char '#'
     s <- bits
     return (ConstantBits s)


----------------------------------------------------------------------
-- Parse registers
----------------------------------------------------------------------

-- A RegisterSrc represents the specification of a register in source
-- code.  A register number can be represented in assembly language as
-- either a hexadecimal character or a decimal string (which should
-- denote a value between 0 and 15, although the range check is
-- performed later).  Examples: R2, r13, ra.

    
-- Parse a string as a register; either succeed with a Register or
-- fail.

register :: Parser RegisterSrc
register = try regDecNumber <|> regHexNumber
-- register =  regHexNumber <|> regDecNumber

-- The register number can be a single hexadecimal digit

regHexNumber :: Parser RegisterSrc
regHexNumber =
  do r <- oneOf "rR"
     x <- hexDigit
     return (RegHex x)

-- Alternatively, the register number can be a string of decimal
-- digits; later we'll have to check that this represents a valid
-- register number.

regDecNumber :: Parser RegisterSrc
regDecNumber =
  do r <- oneOf "rR"
     x <- digit
     xs <- many digit
     return (RegDec (x:xs))

----------------------------------------------------------------------
-- Parse names
----------------------------------------------------------------------

name :: Parser String
name =
  do x <- letter
     xs <- many (letter <|> digit <|> char '_')
     return (x:xs)



----------------------------------------------------------------------
-- Lexical parsing
----------------------------------------------------------------------

lexer :: TokenParser ()
lexer =
  makeTokenParser
   (haskellDef
     { reservedNames = []
     , reservedOpNames = ["+", "-"]
     , nestedComments = True
     , caseSensitive = True
     })

asmWhiteSpace = whiteSpace lexer
asmSymbol     = symbol lexer
asmNatural    = natural lexer
asmIdentifier = identifier lexer

-- endLine succeeds if there is nothing but white space until the end
-- of the line.

-- endLine :: Parser ()
-- endLine = comment <|> end

eol :: Parser ()
eol =
  do char '\n'
     return ()

endLine :: Parser ()
endLine = try comment <|> eol

restOfLine :: Parser String
restOfLine =
  do xs <- many anyChar
     eof
     return xs


{- now in Syntax.hs

whiteSpaceChar :: Parser ()
whiteSpaceChar =
  do oneOf listWhiteSpaceChars
     return ()

-- Skip over any white space, if any

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  do many whiteSpaceChar
     option () comment
     return ()
-}


requireWhiteSpace :: Parser ()
requireWhiteSpace = whiteSpaceChar <|> endLine

nonWhiteSpaceChar :: Parser Char
nonWhiteSpaceChar =
  do x <- noneOf ('\n' : ';' : listWhiteSpaceChars)
     return x

nonWhite :: Parser String
nonWhite =
  do xs <- many nonWhiteSpaceChar
     return xs

{- A comment begins with a semicolon, and extends to the end of the
line. -}

comment :: Parser ()
comment =
  do char ';'
     many anyChar
     return ()

----------------------------------------------------------------------
-- Words
----------------------------------------------------------------------

showObject :: Address16 -> [Hex4] -> String
showObject a xs =
  "address " ++ inthexok 4 a ++ "\n"
  ++ concat (map f xs)
  where f x = "data " ++ x ++ "\n"

----------------------------------------------------------------------
-- Assembling operation field
----------------------------------------------------------------------

lookupMnemonic
  :: OperationField -> [InstrProperties] ->  Maybe InstrProperties
lookupMnemonic NoMnemonic ps = Nothing
lookupMnemonic MnemonicBadSyntax ps = Nothing
lookupMnemonic (Mnemonic im) [] = Nothing
lookupMnemonic (Mnemonic im) (x:xs) =
  let (m,op,fmt,size) = x
  in if map toLower im == m
       then Just x
       else lookupMnemonic (Mnemonic im) xs


----------------------------------------------------------------------
-- Lexical components of assembly language
----------------------------------------------------------------------

evalConst :: Constant -> Word16
evalConst (ConstantI i) = fromIntegral i
evalConst (ConstantHex xs) = hex4Word16 xs
evalConst (ConstantBits bs) = bitsToWord16 bs


----------------------------------------------------------------------
-- Low level formatting
----------------------------------------------------------------------

{- make a string at least k characters wide: fillString k xs will pad
xs with enough leading spaces to ensure that the result has length k
or greater. -}

fillString :: Int -> String -> String
fillString k xs =
  xs ++ take (k - length xs) (repeat ' ')

----------------------------------------------------------------------
-- Parse object module
----------------------------------------------------------------------

{- The assembler produces object statements of type ObjStmt, and these
are converted to text for display.  The linker also produces object
statements.  The linker, and also the loader, need to parse object
code in text form back to the ObjStmt form.  The loader traverses the
ObjStmts and places the data into the memory.  The object module
parser is defined along with the assembler, as it shares a number of
parsing functions. -}

objMod :: Parser ObjStmt
objMod =
  do string "module"
     skipWhiteSpace
     s <- name
     return (ObjModule s)

objWord :: Parser ObjectWord
objWord =
  do c <- anyChar
     char '.'
     x <- word16
     return ((if c=='R' then ObjRel else ObjFixed) x)

objData :: Parser ObjStmt
objData =
  do string "data"
     skipWhiteSpace
     xs <- many objWord
     return (ObjData xs)

objImport :: Parser ObjStmt
objImport =
  do string "import"
     skipWhiteSpace
     s <- name
     return (ObjImport s)

objExport :: Parser ObjStmt
objExport =
  do string "export"
     skipWhiteSpace
     x <- word16
     skipWhiteSpace
     s <- name
     return (ObjExport (ObjFixed x) s)

objOrg :: Parser ObjStmt
objOrg =
  do string "org"
     skipWhiteSpace
     x <- word16
     return (ObjOrg x)

objStmt :: Parser ObjStmt
objStmt = objMod <|> objData <|> objImport <|> objExport <|> objOrg

readObjStmt :: String -> ([String], ObjStmt)
readObjStmt s =
  case parse objStmt "" s of
    Left err -> (lines (show err), ObjData [])
    Right x -> ([], x)

readObjTxt :: String -> ([String], [ObjStmt])
readObjTxt xs =
  let ys = map readObjStmt (lines xs)
      errs = concat (map fst ys)
      stmts = map snd ys
  in (errs,stmts)


----------------------------------------------------------------------
----------------------------------------------------------------------
-- Syntax for words
----------------------------------------------------------------------

parsebit :: Parser Char
parsebit = char '0' <|> char '1'

bits :: Parser String
bits = many parsebit

hexDig :: Parser Char
hexDig = oneOf "0123456789abcdefABCDEF"


word16 :: Parser Word16
word16 =
  do skipWhiteSpace
     a <- hexDig
     b <- hexDig
     c <- hexDig
     d <- hexDig
     skipWhiteSpace
     return (hex4word16 (hexval a) (hexval b) (hexval c) (hexval d))

hex4word16 :: Int -> Int -> Int -> Int -> Word16
hex4word16 a b c d =
  let a1 = shiftL (fromIntegral a) 12
      b1 = shiftL (fromIntegral b)  8
      c1 = shiftL (fromIntegral c)  4
      d1 =        (fromIntegral d)
  in a1 .|. b1 .|. c1 .|. d1

hexString :: Parser String
hexString = many hexDig


----------------------------------------------------------------------
-- Syntax for object modules
----------------------------------------------------------------------

-- parse executable module.  Convert a list of strings representing
-- hex constants to a list of Word16.  This is a basic version,
-- doesn't handle org, and doesn't parse the input or check for valid
-- syntax

parseLoadModule :: String -> ([String], LoadModule)
parseLoadModule lmtext =
  case (parse loadModule "" lmtext) of
    Left err -> (lines (show err), [])
    Right x -> ([], x)


loadModule :: Parser LoadModule
loadModule =
  do xs <- many loadModuleEntry
     return xs

loadModuleEntry :: Parser LoadModuleEntry
loadModuleEntry
  = try loadModuleOrg <|> try loadModuleRel <|> loadModuleData


loadModuleRel :: Parser LoadModuleEntry
loadModuleRel =
  do skipWhiteSpace
     string "relocate"
     x <- word16
     return (LoadModuleRel x)


loadModuleOrg :: Parser LoadModuleEntry
loadModuleOrg =
  do skipWhiteSpace
     string "org"
     x <- word16
     return (LoadModuleOrg x)

loadModuleData :: Parser LoadModuleEntry
loadModuleData =
  do skipWhiteSpace
     x <- word16
     return (LoadModuleData x)

-- Similar version in assembler; need to rationalise this

-- White space separates the fields of a statement, and can appear at
-- the end of a line.  A line may consist of nothing but white space.

listWhiteSpaceChars :: String
listWhiteSpaceChars = [' ', '\t', '\r', '\n']

skipWhiteSpace :: Parser ()
skipWhiteSpace =
  do many whiteSpaceChar
     return ()

whiteSpaceChar :: Parser ()
whiteSpaceChar =
  do oneOf  [' ', '\t', '\r', '\n']
     return ()

{-
char 'h' >> return CmdHelp
       <|> return (CmdUnparseable "no command")
      <|> (do char 'f'; return (CmdFile "notyet"))
try cmdHelp <|> try cmdSetSrcFile
  <|> cmdUnparseable

-}

