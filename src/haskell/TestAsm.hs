module TestAsm where

{-
Interactive testing, enter the source directory, then:
  ghci
  :load Arith
  :add Arch
  :add Assembler
  :add TestAsm
-}

import HDL.Hydra.Sigma16.Arith
import HDL.Hydra.Sigma16.Arch
import HDL.Hydra.Sigma16.Assembler

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language


------------------------------------------------------------------------
-- Test asm

run_obj x =
  do let (errs, stmt) = readObjStmt x
     putStrLn (concat errs)
     putStrLn (showObjStmt stmt)

run_asm xs =
  do putStrLn "running assembler"
     let (nextline, symtab, codelines,
          (listing,nerrs,errlocs), objmodule) = asm xs
     putStrLn "printing symbol table"
     putStrLn (concat (showSymTab symtab))
     putStrLn "printing code lines"
     putStrLn (showCodeLines codelines)
     putStrLn "printing object module"
     putStrLn objmodule

-- test assembler directives
test_dir = run_asm xs where
 xs = [ "myprog  module"
      , "        export x            ; maybe useful elsewhere!"
      , "zz      import              ; defined elsewhere"
      , "asize   equ    5            ; size of an array"
      , "        load   R13,arr[R8]  ; forward reference"
      , "        add    R3,R1,R8     ; do something"
      , "        store  R9,zz[R3]    ; use imported varaible"
      , "x       data  123           ; initialized variable"
      , "arr     org   *+asize       ; leave space for array"
      , "y       data  2+3           ; fixed expression"
      ]

-- test instructions and expressions
test_relexpr = run_asm xs where
 xs = [ "        add  R1,R2,R3       ; an addition"
      , "x       data  123           ; initialized variable"
      , "        load  R13,loop[R8]  ; forward reference"
      , "y       data  2+3           ; fixed expression"
      , "loop    load  R5,x[R12]     ; variable reference"
      , "        data  x+3           ; relocatable expression"
      , "        data  10*(y-x)      ; fixed expression"
      ]

-- test_cj:  cmp, jump
test_cj = run_asm xs where
 xs = [ "        cmp     R9,R3       ; a comparison"
      , "        jumpne  loop[R0]    ; conditional jump"
      , "        jumplt  y[R8]       ; different condition"
      , "y       data    2+3         ; fixed expression"
      , "loop    load    R5,y[R12]   ; variable reference"
      ]

{-
;      , "loop    sub  R4,R5,R6     ; a subtraction"
-}

------------------------------------------------------------------------
-- Test expressions

test_expr =
  do putStrLn "Test expressions"
     testparse expr "123"
     testparse expr "$a29c"
     testparse expr "a29c"
     testparse expr "*+5"
     testparse expr "a+b+c"
     testparse expr "a+b*c"
     testparse expr "(a+b)*c"
     testparse expr "&a*2"





------------------------------------------------------------------------
-- Unit testing
------------------------------------------------------------------------


-- A function for testing and debugging the parser

testparse p s =
  do putStrLn ("Input: " ++ s)
     case (parse p "" s) of
       Left err -> do putStr "error "
                      print err
       Right x -> do putStr "  success: "
                     print x

{-
-- useParse :: (MonadError [Char] m) => GenParser tok () a -> [tok] -> m a
This gives error with gtk3...

src\HDL\Hydra\Sigma16\Assembler.hs:1513:1:
    Non type-variable argument
      in the constraint: Text.Parsec.Prim.Stream
                           s Data.Functor.Identity.Identity t
    (Use FlexibleContexts to permit this)
    When checking that `useParse' has the inferred type
      useParse :: forall s t (m :: * -> *) a.
                  (Monad m,
                   Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t) =>
                  Text.Parsec.Prim.Parsec s () a -> s -> ErrorT [Char] m a

useParse p xs =
  case (parse p "" xs) of
    Left err -> throwError ("Parse error: " ++ show err)
    Right x -> return x
-}




-- A main program for quick testing.  To run the suite of unit tests,
-- launch ghci, load Assembler, and enter go.

go :: IO ()
go = run_unit_tests

-- Run the unit tests for all the sections of the assembler as one big
-- batch. The various operations "run_unit_test_foo" are defined in
-- the relevant parts of the program, and they serve to provide
-- examples of the language constructs.

run_unit_tests :: IO ()
run_unit_tests =
  do print_header_line "Collection of unit tests"
     run_unit_test_get_fields
--     run_unit_test_label
     run_unit_test_operation
     run_unit_test_register
--     run_unit_test_identop
--     run_unit_test_constop
     run_unit_test_RRR
     run_unit_test_RX

-- Prints a label, so each section of testing can identify itself

print_header_line :: String -> IO ()
print_header_line xs =
  do let dashline = take 72 (repeat '-')
     putStrLn ""
     putStrLn dashline
     putStrLn xs
     putStrLn dashline
     putStrLn ""

dashes = take 50 (repeat '-')

--------------------------------------------------
-- Test assembler program

unit_test_assembler :: IO ()
unit_test_assembler =
  do putStrLn dashes
     putStrLn "Unit test assembler"
--     test_name
     run_unit_test_get_fields
--     run_unit_test_label_field
     test_operationField
--     test_operand_k
--     unit_test_operand_RRR
--     test_operand_RX
     test_operandField
--     test_pass1 testdata0
--     test_pass1 testdata1
--     test_pass2 testdata0
--     test_pass2 testdata1
--     assemble_io "testdata1" testdata1
     putStrLn "End of assembler unit testing"
     putStrLn dashes


-- Test data for assembler

testdata0 =
  [ "loop  add   R1,R2,R3"
  , "      load  R1,x[R0]      ; a comment"
  , "skip  jump  R1,loop[R0]   another comment"
  ]

testdata1 =
  [ "lab1 add   R1,R2,R3"
  , "     load  R1,x[R0]  ; hello"
  , "loop store R1,y[R0]  ; hello"
  , "     add   R5,R6,R7  ; more"
  , "next sub   R5,R6,R7  ; more"
  , "x    data  456"
  , "y    data  123"
  , "   "
  ]

testdata2 =
  [ ""
  , ";comment"
  , " ;comment"

  , "label"
  , "label;comment"
  , "label ; comment"

  , "  "
  , ";comment "
  , " ;comment "

  , "  operation"
  , "  operation;comment"
  , "label operation"
  , "label operation;comment"

  , "  operation "
  , "  operation ;comment"
  , "label operation ;comment"

  , "  operation operands"
  , "  operation operands;comment"
  , "label operation operands"
  , "label operation operands;comment"

  , "  operation operands "
  , "  operation operands ;comment"
  , "label operation operands "
  , "label operation operands ;comment"

  , "  operation operands  badcomment error"
  , "label operation operands badcomment error"

  , "     load R1,x[R0]  ; hello"
  , "loop load R1,x[R0]  ; hello"
  , "     add  R5,R6,R7  ; more"
  , "loop add  R5,R6,R7  ; more"
  , "     data 123"
  , "xy   data 123"
  ]

testdata3 =
  [ "      data   5"
  , "      jumpf  R2,loop[R1]"
  , "xy    data   0xc034"
  , "      data   xy"
  , "loop  add    R5,R6,R7"
  , "skip  sub    Ra,Rb,Rc"
  , "      load   R1,xy[R0]"
  , "      add    R0,R1,R2"
  ]


--------------------------------------------------
-- Testing for Pass 1

run_unit_test_pass1 :: IO ()
run_unit_test_pass1 =
  do print_header_line "Unit test for pass 1"
     putStrLn "testdata0"
     unit_test_pass1 testdata0

unit_test_pass1 :: [String] -> IO ()
unit_test_pass1 xs =
  do putStrLn dashes
     putStrLn "test_pass1"
     putStrLn "Input:"
     putStrLn (concat (map (++"\n") xs))
     let (a, st, cls) = pass1 xs
     putStrLn ("Final address = " ++ show a)
     putSymTab st
     putStrLn "Code:"
     putStrLn (showCodeLines cls)




--------------------------------------------------
-- Testing for Pass 2

unit_test_pass2 :: [String] -> IO ()
unit_test_pass2 xs =
  do putStrLn dashes
     putStrLn "test_pass2"
     putStrLn "Input:"
     putStrLn (concat (map (++"\n") xs))
     let (a, st, cls) = pass1 xs
     let (st',cls') = pass2 st cls
     putStrLn ("Final address = " ++ show a)
     putSymTab st
     putStrLn "Code:"
     putStrLn (showCodeLines cls)

--------------------------------------------------
-- Test: Locate the fields of an assembly language statement

run_unit_test_get_fields :: IO ()
run_unit_test_get_fields =
  do 
     print_header_line "Unit test getFields"
     testparse getFields "label mnemonic operands this is comment"
     testparse getFields "12 34 45 78"
     testparse getFields " 34 45 78"
     testparse getFields " 34 45"
     testparse getFields " 34"
     testparse getFields " "
     testparse getFields ""
     testparse getFields " 34; 45"
     testparse getFields " 34 ; 45"
     testparse getFields "ab cd ef gh"



--------------------------------------------------
-- Testing for operation field

test_operationField =
  do putStrLn dashes
     putStrLn "test_operationField"
     testparse operationField ""
     testparse operationField "*?ab"
     testparse operationField "5dogs"

run_unit_test_operation :: IO ()
run_unit_test_operation =
  do print_header_line "Unit test operation field"
     unit_test_operation "load"
     unit_test_operation "macroname"
     unit_test_operation "load23"
     unit_test_operation "34"
     unit_test_operation "load+store"
     unit_test_operation "no*good"
     unit_test_operation ""

unit_test_operation :: String -> IO ()
unit_test_operation xs =
  do putStrLn ("<<" ++ xs ++ ">>")
     let (errs,y) = buildOperationField xs
     putStrLn ("  Result: " ++ show y)
     putStrLn ("  " ++ show (length errs) ++ " error lines:")
     putStrLn (concat ["    <<" ++ e ++ ">>\n" | e <- errs])


--------------------------------------------------
-- Test operand field

--------------------------------------------------
-- Testing RRR operands

run_unit_test_RRR :: IO ()
run_unit_test_RRR =
  do print_header_line "Unit test RRR operand field"
     unit_test_RRR "R3,R4,R5"
     unit_test_RRR "R0,Rc,R9"
     unit_test_RRR "R3,R?,R5"
     unit_test_RRR "r3,r4,r5"
     unit_test_RRR "ra,R0,R1"
     unit_test_RRR "r14,r0,r0"
     unit_test_RRR "5,6,7"
     unit_test_RRR "R1,label[R2]"
     unit_test_RRR "R1,R2,R3"
     unit_test_RRR "R1,R2,R3,extrajunk"
     unit_test_RRR ",R1,R2,R3"
     unit_test_RRR "R1,,R2,R3"
     unit_test_RRR "R1,R2,,R3"
     unit_test_RRR "R1,R2,R3,"
     unit_test_RRR "dog,R2,R3"
     unit_test_RRR "R1,dog,R3"
     unit_test_RRR "R1,R2,dog"
     unit_test_RRR ""

-- Perform an RRR unit test on a string

unit_test_RRR :: String -> IO ()
unit_test_RRR xs =
  do putStrLn ("Trying RRR on <<" ++ xs ++ ">>")
     case build_RRR xs of
       RRR_syntax_ok d a b ->
         do putStrLn ("  RRR syntax ok: " ++ show d ++ " " 
                        ++ show a ++ " " ++ show b ++ "\n")
       RRR_syntax_err errs ->
         do putStrLn ("  " ++ show (length errs) ++ " error lines:")
            putStrLn (concat ["    <<" ++ e ++ ">>\n" | e <- errs])


--------------------------------------------------
-- Testing  RX operand

run_unit_test_RX :: IO ()
run_unit_test_RX =
  do print_header_line "Unit test RX operand field"
     unit_test_RX "R2,variable[R0]"
     unit_test_RX "R1,234[Ra]"
     unit_test_RX "R1,abc[R2]"

unit_test_RX :: String -> IO ()
unit_test_RX xs =
  do putStrLn ("Trying RX on <<" ++ xs ++ ">>")
     case build_RX xs of
       RX_syntax_ok d a ->
         do putStrLn ("  RX succeeded: " ++ show d ++ " " 
                        ++ show a ++ "\n")
       RX_syntax_err errs ->
         do putStrLn ("  " ++ show (length errs) ++ " error lines:")
            putStrLn (concat ["    <<" ++ e ++ ">>\n" | e <- errs])

-- X operand



--------------------------------------------------
-- Testing for operand field

{-
unitTestFields :: IO ()
unitTestFields =
  do putStrLn "Statement field unit tests"
     putStrLn "Strings are <<delimited like this>>"
     unitTestOperand "R1,R2,R3"
     unitTestOperand "123"
     unitTestOperand "$123"
     unitTestOperand "R2,xyz[R4]"
     unitTestOperand "R2,5[R4]"
     unitTestOperand "R2,$abcd[R4]"
     unitTestOperand "1,2,3"
     unitTestOperand "1,2,x"
     unitTestOperand "R1,R2,x"
     unitTestOperand "abc"
     unitTestOperand "R2,$abcd,R4"
     unitTestOperand ""
-}

-- Operand field (old version)


unitTestOperand :: String -> IO ()
unitTestOperand xs =
  do putStrLn xs
     let (errs,y) = buildOperandField xs
     putStrLn ("  Result: " ++ show y)

test_operandField = undefined


{-
  do putStrLn dashes
     putStrLn "test_operandField"
     testparse operandField "1234"
     testparse operandField "xyz"
     testparse operandField "R1,R2,R3"
     testparse operandField "R1,234[Ra]"
     testparse operandField "R1,abc[R2]"
     testparse operandField "*?$"
-}


{- testing...
buildOperandField "0"
buildOperandField "-0"
buildOperandField "1234"
buildOperandField "-1234"
buildOperandField "0xab23"
buildOperandField "0b0011010100001111"
buildOperandField "xyz"
buildOperandField "R1,R2,R3"
buildOperandField "R1,234[Ra]"
buildOperandField "R1,abc[R2]"
buildOperandField "*?$"
-}


--------------------------------------------------
-- Testing registers

run_unit_test_register =
  do print_header_line "Unit test register"
     unit_test_register "R3"
     unit_test_register "r3"
     unit_test_register "Ra"
     unit_test_register "ra"
     unit_test_register "r*"
     unit_test_register "R15"
     unit_test_register "Rab"
     unit_test_register "junk"

unit_test_register :: String -> IO ()
unit_test_register x =
  do testparse register x
     return ()

--------------------------------------------------
-- Testing constants


run_unit_test_constop :: IO ()
run_unit_test_constop =
  do print_header_line "Unit test constant operand"
     unit_test_constop "234"
     unit_test_constop "$2a4c"
     unit_test_constop "2a4c"
     unit_test_constop "varname"
     unit_test_constop ""

unit_test_constop :: String -> IO ()
unit_test_constop xs =
  do putStrLn ("Trying Constant operand on <<" ++ xs ++ ">>")
     case build_Constop xs of
       Constop_syntax_ok v ->
         do putStrLn ("  Constant syntax ok: " ++ show v
                        ++ "\n")
       Constop_syntax_err errs ->
         do putStrLn ("  " ++ show (length errs) ++ " error lines:")
            putStrLn (concat ["    <<" ++ e ++ ">>\n" | e <- errs])


test_constant =
  do putStrLn dashes
     putStrLn "test_constant"
     testparse constant "123"
     testparse constant "-123"
     testparse constant "0"
     testparse constant "-0"
     testparse constant "0x1ab2"
     testparse constant "0b0011"
     testparse constant "0x0011"
     testparse constant "abc"
     testparse constant "0xabgh"
     testparse constant "0b0123"



--------------------------------------------------
-- Testing names


run_unit_test_name :: IO ()
run_unit_test_name =
  do putStrLn dashes
     putStrLn "test_name"
     testparse name "abc"
     testparse name "abc123"
     testparse name "ab_c1_23"
     testparse name "abc*123"



--------------------------------------------------
-- Testing for label field

run_unit_test_label :: IO ()
run_unit_test_label =
  do print_header_line "Unit test label field"
     unit_test_label "goodlabel"
     unit_test_label "x"
     unit_test_label "good_label"
     unit_test_label "ab_c32a_vb24"
     unit_test_label "verylongidentifier_itgoesonandon"
     unit_test_label "Abc"
     unit_test_label "aBbc"
     unit_test_label "bad/label"
     unit_test_label "123xyz"
     unit_test_label "123"
     unit_test_label "$a123"
     unit_test_label ""
     unit_test_label "a"
     unit_test_label "loop"
     unit_test_label "xyz123"
     unit_test_label "VeryLongLabelItIsReallyTooLong"
     unit_test_label ""
     unit_test_label "this should be impossible after splitting fields"
     unit_test_label "123"
     unit_test_label "*?ab"
     unit_test_label "5dogs"
     unit_test_label "?*3"
     unit_test_label "xy*?ab"

unit_test_label :: String -> IO ()
unit_test_label xs =
  do putStrLn ("<<" ++ xs ++ ">>")
     case buildLabelField xs of
       Label s ->
         do putStrLn ("  Result is label <<" ++ s ++ ">>")
       LabelBadSyntax errs ->
         do putStrLn ("  " ++ show (length errs) ++ " error lines:")
            putStrLn (concat ["    <<" ++ e ++ ">>\n" | e <- errs])
       LabelAbsent ->
         do putStrLn "  Label is absent"



--------------------------------------------------
-- Testing for code generator

{- testing
genCon [] (Operand_k (ValCon (ConstantHex "a12c")))
genCon [] (Operand_k (ValCon (ConstantHex "1234")))
genCon [] (Operand_k (ValCon (ConstantI 35)))
genCon [] (Operand_k (ValSym "xyz"))
genCon [SymbolDef {symName="xyz",symVal=123, defLines=[],useLines=[]}] (Operand_k (ValSym "xyz"))
-}


{- test cases
val16_code (ValCon (ConstantHex "a12c"))
-}

