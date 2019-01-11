
const32ffffffff, const320000ffff, const32ffff0000 :: Word32
const32ffffffff = complement 0
const320000ffff = Data.Bits.shift const32ffffffff (-16)
const32ffff0000 = Data.Bits.shift const32ffffffff 16


----------------------------------------------------------------------
-- Unit testing
----------------------------------------------------------------------

test_valHex4 =
  do putStrLn dashes
     putStrLn "test_valHex4"
     putStrLn (showEither (valHex4 "0005"))
     putStrLn (showEither (valHex4 "0050"))
     putStrLn (showEither (valHex4 "0500"))
     putStrLn (showEither (valHex4 "5000"))
     putStrLn (showEither (valHex4 "002c"))
     putStrLn (showEither (valHex4 "12345"))
     putStrLn (showEither (valHex4 "123"))
     putStrLn (showEither (valHex4 "abcd"))
     putStrLn (showEither (valHex4 "abgd"))

showEither :: Show a => Either String a -> String
showEither (Left msg) = "Error: " ++ msg
showEither (Right x) = "OK: " ++ show x




my_print_header_line xs = putStrLn xs -- ??? should import

dashes :: String
dashes = take 50 (repeat '-')



----------------------------------------------------------------------
-- Testing arithmetic operations
----------------------------------------------------------------------

testArith =
  test_addtc

test_addtc = runtests
  [ testArithOp21 "addtc 0 0 0"             addtc 0 0 0
  , testArithOp21 "addtc 0 2 3"             addtc 0 2 3
  , testArithOp21 "addtc 63 2 3"            addtc 63 2 3
  , testArithOp21 "addtc 0 (-2) 3"          addtc 0 (-2) 3
  , testArithOp21 "addtc 0 2 (-3)"          addtc 0 2 (-3)
  , testArithOp21 "addtc 0 (2^15) (2^15)"   addtc 0 (2^15) (2^15)
  , testArithOp21 "addtc 0 (2^14) (2^14)"   addtc 0 (2^14) (2^14)
  , testArithOp21 "addtc 0 65533 0"         addtc 0 65533 0
  , testArithOp21 "addtc 0 65533 1"         addtc 0 65533 1
  , testArithOp21 "addtc 0 65533 2"         addtc 0 65533 2
  , testArithOp21 "addtc 0 65533 3"         addtc 0 65533 3
  , testArithOp21 "addtc 0 65533 2"         addtc 0 65533 2
  , testArithOp21 "addtc 0 65533 3"         addtc 0 65533 3
  ]

test_subtc = runtests
  [ testArithOp21 "subtc 0 0 0"             subtc 0 0 0
  , testArithOp21 "subtc 0 12 15"           subtc 0 12 15
  , testArithOp21 "subtc 0 15 12"           subtc 0 15 12
  ]

test_cmp = runtests
  [ testArithOp20 "cmptc 0 0 0"             cmptc 0 0 0
  , testArithOp20 "cmptc 0 12 15"           cmptc 0 12 15
  , testArithOp20 "cmptc 0 12 12"           cmptc 0 12 12
  , testArithOp20 "cmptc 0 15 12"           cmptc 0 15 12
  ]
  
runtests [] = return ()
runtests (x:xs) = putStrLn x >> runtests xs

testArithOp21
  :: String -> ArithOp21 -> Word16 -> Word16 -> Word16 -> String
testArithOp21 src f cc a b =
  let (cc',x) = f cc a b
  in src
       ++ "\nOperands"
       ++ "\n  cc  = " ++ word16Hex cc  ++ "  " ++ showCCbits cc
       ++ "\n  a   = " ++ showHexBinTc a
       ++ "\n  b   = " ++ showHexBinTc b
       ++ "\nResults"
       ++ "\n  cc' = " ++ word16Hex cc' ++ "  " ++ showCCbits cc'
       ++ "\n  x   = " ++ showHexBinTc x
       ++ "\n"

testArithOp20
  :: String -> ArithOp20 -> Word16 -> Word16 -> Word16 -> String
testArithOp20 src f cc a b =
  let cc' = f cc a b
  in src
       ++ "\nOperands"
       ++ "\n  cc  = " ++ word16Hex cc  ++ "  " ++ showCCbits cc
       ++ "\n  a   = " ++ showHexBinTc a
       ++ "\n  b   = " ++ showHexBinTc b
       ++ "\nResults"
       ++ "\n  cc' = " ++ word16Hex cc' ++ "  " ++ showCCbits cc'
       ++ "\n"

unit_test_showWord16bin :: (Integral a, Show a) => a -> IO ()
unit_test_showWord16bin x =
  do let y = (fromIntegral x) :: Word16
     let z = showWord16bin y
     putStrLn ("Input " ++ show x ++ " shown as " ++ z)

run_unit_test_showWord16bin =
  do putStrLn "Unit test showWord16bin"
     unit_test_showWord16bin (50000 :: Integer)
     unit_test_showWord16bin (2^16 - 1 :: Integer)
     unit_test_showWord16bin (2^16 :: Integer)
     unit_test_showWord16bin (2^16 + 1 :: Integer)
     unit_test_showWord16bin (2^17 - 2 :: Integer)
     unit_test_showWord16bin (2^17 - 1 :: Integer)
     unit_test_showWord16bin (2^17 :: Integer)
     unit_test_showWord16bin (0 :: Integer)
     unit_test_showWord16bin ((-1) :: Integer)
     unit_test_showWord16bin ((-2) :: Integer)
     unit_test_showWord16bin ((-(2^15)) :: Integer)
     unit_test_showWord16bin ((-(2^15) + 1) :: Integer)
     unit_test_showWord16bin (3^50 :: Integer)

unit_test_showWord16tc :: (Integral a, Show a) => a -> IO ()
unit_test_showWord16tc x =
  do let y = (fromIntegral x) :: Word16
     let z = showWord16tc y
     putStrLn ("Input " ++ show x ++ " shown as " ++ z)

run_unit_test_showWord16tc =
  do putStrLn "Unit test showWord16tc"
     unit_test_showWord16tc (0 :: Integer)
     unit_test_showWord16tc (1 :: Integer)
     unit_test_showWord16tc (2 :: Integer)
     unit_test_showWord16tc (3 :: Integer)
     unit_test_showWord16tc ((-1) :: Integer)
     unit_test_showWord16tc ((-2) :: Integer)
     unit_test_showWord16tc ((-3) :: Integer)
     unit_test_showWord16tc (20000 :: Integer)
     unit_test_showWord16tc ((-20000) :: Integer)
     unit_test_showWord16tc (2^15 - 3 :: Integer)
     unit_test_showWord16tc (2^15 - 2 :: Integer)
     unit_test_showWord16tc (2^15 - 1 :: Integer)
     unit_test_showWord16tc (2^15 :: Integer)
     unit_test_showWord16tc (2^15 + 1 :: Integer)
     unit_test_showWord16tc (2^15 + 2 :: Integer)
     unit_test_showWord16tc ((-(2^15) - 2) :: Integer)
     unit_test_showWord16tc ((-(2^15) - 1) :: Integer)
     unit_test_showWord16tc ((-(2^15)) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 1) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 2) :: Integer)
     unit_test_showWord16tc ((-(2^15) + 3) :: Integer)


run_unit_test_addWord16 :: IO ()
run_unit_test_addWord16 =
  do my_print_header_line "Unit test addWord16"
     unit_test_addWord16 2 3
     unit_test_addWord16 (2^14) (2^14)
     unit_test_addWord16 (2^14 + 3) (2^14 + 5)

unit_test_addWord16 :: Int -> Int -> IO ()
unit_test_addWord16 x y =
  do let z = addWord16 (fromIntegral x) (fromIntegral y)
     putStrLn (show x ++ " + " ++ show y ++ " => " ++ show z)

