 module Arithmetic where

-- Define operations on the primitive data types: signed and unsigned
-- integers, Booleans, and the condition code.  The operations include
-- arithmetic and logic, as well as number conversions.

import Data.Word
import Data.Bits
import Data.List

----------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------

data ArithException
  = NoException
  | ArithOverflow
  | ArithZeroDivide
  deriving Show

----------------------------------------------------------------------
-- Words
----------------------------------------------------------------------

-- The architecture uses 16 bit words for registers, and 4-bit words
-- for the fields of an instruction and as addresses into the register
-- file.

-- Word16 and Word8 are defined in module Data.Word
type Word4 = Word8
type Address16 = Word16
type Bit = Word16

-- Some useful word constants

const16ffff, const1600ff, const16000f, const16ff00 :: Word16
const16ffff = complement 0
const1600ff = Data.Bits.shift const16ffff (-8)
const16ff00 = Data.Bits.shift const16ffff 8
const16000f = Data.Bits.shift const16ffff (-12)

-- Sigma 16 uses 16-bit words.  These may represent a binary number
-- (used for addresses) or a two's complement number (used for
-- ordinary integer variables).

-- Binary numbers (16 bit word)
--   0 <= x <= 2^16-1
--   0, 1, ..., 65535

-- Two's complement numbers (16 bit word)
--   -2^15 <= x <= 2^15-1
--   -32768, -32767, ..., -1, 0, 1, 2, ..., 32767

maxtc16 :: Int
maxtc16 = 2^15 - 1

-- Sign.  Given a word, its most significant bit determines the sign:
-- if 1, the number is negative, if 0, the number is non-negative.  If
-- we have an Int representation x of the number, then the two's
-- complement value is negative if x >= 2^15.

-- Two's complement value.  Let bin be the value of a word assuming
-- binary representation.  Then the value assuming two's complement
-- representation is

-- binval and tcval interpret a 16-bit word as an integer (binary and
-- two's complement respectively) and returns the value as an Int.
--    tcval 100...0  = 2^15 - 2^16 = 2^15 - 2 * 2*15 = -2^15
--    tcval 111...1  = 2^16-1 - 2^16 = -1

binval, tcval :: Word16 -> Int
binval x = fromIntegral x
tcval x =
  let y = (fromIntegral x) :: Int
  in if y >= 2^15
       then y - 2^16
       else y

----------------------------------------------------------------------
-- Fields and bits
----------------------------------------------------------------------

-- Fields and bits.  There are functions to build a word from fields,
-- and to extract a field or a bit in a word, given the start and end
-- bit positions.

-- Big-endian or little endian?  Big-endian notation has bit 0 in the
-- leftmost (most significant) position, while little-endian uses
-- index 0 for the least significant bit.  The Haskell Data.Bits
-- convention is little-endian.  The bit-numbering convention for
-- Sigma16 is "little-endian" notation, with bit 0 in the rightmost
-- (least significant) position. This is also the convention used for
-- the Haskell Standard Prelude functions on words.

-- Build a 16-bit word from four 4-bit words
buildword :: Integral a => (a,a,a,a) -> Word16
buildword (a,b,c,d) =
 fromIntegral a * 16^3 + fromIntegral b * 16^2 +
  fromIntegral c * 16^1 + fromIntegral d * 16^0

-- Split a 16-bit word into four 4-bit words

splitword :: Word16 -> (Word4,Word4,Word4,Word4)
splitword w =
  let w1 = w `div` 16
      w2 = w1 `div` 16
      w3 = w2 `div` 16
  in (fromIntegral (w3 `mod` 16),
      fromIntegral (w2 `mod` 16),
      fromIntegral (w1 `mod` 16),
      fromIntegral (w `mod` 16))

-- Extract a field from a word, given the start and end positions:
-- extract a b x gives a field of length b-a+1 bits from x, starting
-- in bit position a and ending in bit position b.

extract :: (Integral a, FiniteBits a) => Int -> Int -> a -> a
extract a b x =
  let n = finiteBitSize x
      y = shiftL x a
      z = shiftR y (a + (n-1) - b)
  in z

----------------------------------------------------------------------
-- Arithmetic functions
----------------------------------------------------------------------

-- Arithmetic operations generally take some operands from the
-- register file, and possibly the existing condition code (cc), and
-- produce some result words and possibly a modified condition code.
-- An operation also returns an ArithException (which can be
-- NoException, or a specific arithmetic exception).

-- ArithOp20:  no result word, sets cc (e.g. cmp)
type ArithOp20
  = Word16 -> Word16 -> Word16
  -> (Word16, ArithException)

-- ArithOp21: two operands and cc, produce one result and cc
type ArithOp21
  = Word16 -> Word16 -> Word16
  -> (Word16, Word16, ArithException)

----------------------------------------------------------------------
-- Condition codes
----------------------------------------------------------------------

-- R15 has several roles:
--  (1) many instructions set the condition code, which is R15.
--  (2) division uses R15 to hold the remainder
--  (3) long unsigned multiplication places the upper 16 bits of
--      the product in R15

-- Each condition code flag has a bit index that determines where it
-- is in R15.  The least significant bit has index 0, and the most
-- significant bit has index 15.  Each condition code flag has a
-- one-letter symbol.  The value of CC_x, where x is a flag, is an Int
-- giving the index of the flag in the condition code.

-- The following functions set the condition code to reflect the
-- result of a comparision or arithmetic operation.  Implementation
-- note: the Haskell libraries (testBit etc) number the lsb as bit 0.
-- The convention in Sigma16 is also to number the bits in little
-- endian form, where the least significant bit is bit 0, and the most
-- significant is bit 15.

------------------------------------------------------------
--  Bit  Flag    Meaning
------------------------------------------------------------
--   0     G      > unsigned
--   1     g >    > signed
--   2     E =    = unsigned, signed
--   3     l <    < signed
--   4     L      < unsigned
--   5     P +    positive signed
--   6     Z 0    zero
--   7     N -    negative signed
--   8     V      overflow unsigned
--   9     v      overflow signed
--  10     C      Carry propagation unsigned
------------------------------------------------------------

-- The following constants consist of a 0 word with just one bit set.
-- Several of them can be or'ed together to form a condition code.

cc_G = bit  0 :: Word16
cc_g = bit  1 :: Word16
cc_E = bit  2 :: Word16
cc_l = bit  3 :: Word16
cc_L = bit  4 :: Word16
cc_P = bit  5 :: Word16
cc_Z = bit  6 :: Word16
cc_N = bit  7 :: Word16
cc_V = bit  8 :: Word16
cc_v = bit  9 :: Word16
cc_C = bit 10 :: Word16

-- Define condition code indices for the conditions

cci_C = 10 :: Int
cci_v =  9 :: Int
cci_V =  8 :: Int
cci_N =  7 :: Int
cci_Z =  6 :: Int
cci_P =  5 :: Int
cci_L =  4 :: Int
cci_l =  3 :: Int
cci_E =  2 :: Int
cci_g =  1 :: Int
cci_G =  0 :: Int

-- ccNames is used to show the condition code symbolically; for signed
-- integers the usual symbols < = > are used for comparisons, and - 0
-- + are used for relation with zero.  They are ordered left to right,
-- according to the index for each condition.

ccNames :: [(Int,String)]
ccNames =
  [ (10, "C")
  , ( 9, "v")
  , ( 8, "V")
  , ( 7, "-")
  , ( 6, "0")
  , ( 5, "+")
  , ( 4, "L")
  , ( 3, "<")
  , ( 2, "=")
  , ( 1, ">")
  , ( 0, "G")
  ]

-- selectedCCbit returns the Boolean value of the condition code bit
-- that is specified by the d field of the ir; it can be used to
-- determine whether to perform a conditional jump.

selectedCCbit
  :: Word4   -- jump variant opcode (instruction d field)
  -> Word16  -- condition code (R15)
  -> Bool    -- value of the cc bit specified by dfield
selectedCCbit dfield cc =
  let dint = fromIntegral dfield :: Int
      dsafe = if dint<0 || dint>15 then 0 else dint
  in testBit cc dsafe

-- putCCbit chooses either a word of 0, or a word with a particular cc
-- bit set; the argument x should be cc_G or one of the other similar
-- values.  The result can be or'ed with other words to produce the
-- full condition code.

putCCbit :: Bool -> Word16 -> Word16
putCCbit False x = zeroBits
putCCbit True x = x

putCC :: Bool -> Int -> Word16 -> Word16
putCC False i x = x
putCC True i x = setBit x i

-- showCCbits displays the condition code in symbolic form,
-- which is more readable than hex.

showCCbits :: Word16 -> String
showCCbits cc =
  concat  [y | (i,y) <- ccNames, testBit cc i]

----------------------------------------------------------------------
-- Unsigned binary addition
----------------------------------------------------------------------

addWord16 :: Word16 -> Word16 -> Word16
addWord16 a b = a + b
{-
  let a' = (fromIntegral a) :: Integer
      b' = (fromIntegral b) :: Integer
      x = a' + b' :: Word32
      y = (fromIntegral x) :: Word16
  in y
-}

----------------------------------------------------------------------
-- Signed two's complement addition
----------------------------------------------------------------------

-- Theory of addition.
-- Let r be the correct mathematical sum; thus r = tcval x
-- + tcval y.  Largest possible result magnitude of result:

-- pos + pos  (2^15 - 1) + (2^15 - 1) + 1
--            = 2^16 - 1

-- Addition and overflow

-- If the signs of x and y differ, then x+y cannot produce an
-- overflow.

-- For 4-bit words, the range is -2^(-3) .. 2^3-1  or -8, ..., 7.


{-

Overflow of nonnegative numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  0111 + 0111 =    1110 with carry out = 0
If we have a carry input that can be specified, then the maximum result is
  0111 + 0111 + 1 = 1111 with carry out = 0

When will overflow occur?

The smallest result with overflow is the (positive) result 1000, which
would need an extra bit to represent correctly as 01000.  This number
is 8, i.e. 2^(n-1).

The largest result you can get is 0111 + 0111 + 1, which is (extending
to more bits) 01111.

When adding nonnegative numbers, the binary adder circuit will produce
carry out = 0, and overflow occurs if and only if the msb of the
result is 1.

Overflow of negative numbers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Largest magnituded negative numbers

1000 + 1000  which is -8 + -8 = -16

sum in extended word is  0001 0000
from 4-bit addition, the carry output = 1 and the sign of the result is -

An overflow occurs if the sign of the result is 0

-}

-- Add two's complement with no carry: addtc performs addition without
-- carry propagation.  It can be used on either Natural numbers (16
-- bit binary) or integers (16 bit twos complement); the difference is
-- in the way the condition code is set.

addtc :: ArithOp21
addtc cc a b = (cc', s', excp)
  where a' = (fromIntegral a) :: Word32
        b' = (fromIntegral b) :: Word32
        s = a' + b' :: Word32
        s' = fromIntegral s  -- should truncate, does it?
        cout = testBit s 16
        ovbin = s >= 2^15
        ovtc = zeroBits
--        ovtc =  (if r < -2^15 || r >= 2^15 then 1 else 0) :: Word16
        lt = a' < b'
        eq = a' == b'
        gt = a' > b'
        cc' = putCCbit cout cc_C
              .|. putCCbit ovbin cc_V
              .|. putCCbit ovtc cc_v
              .|. putCCbit lt cc_l
              .|. putCCbit eq cc_E
              .|. putCCbit gt cc_g
        excp = if ovtc then ArithOverflow else NoException

----------------------------------------------------------------------
-- Unsigned addition
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Subtraction
----------------------------------------------------------------------

tcNegate :: Word16 -> Word16
tcNegate x = complement x + 1
  
subtc :: ArithOp21
subtc cc a b = (cc', s', excp)
  where a' = (fromIntegral a) :: Word32
        b' = (fromIntegral (tcNegate b)) :: Word32
        s = a' + b' :: Word32
        s' = fromIntegral s  -- should truncate, does it?
        cout = testBit s 16
        ovbin = s >= 2^15
        ovtc = zeroBits
--        ovtc =  (if r < -2^15 || r >= 2^15 then 1 else 0) :: Word16
        lt = a' < b'
        eq = a' == b'
        gt = a' > b'
        cc' = putCCbit cout cc_C
              .|. putCCbit ovbin cc_V
              .|. putCCbit ovtc cc_v
              .|. putCCbit lt cc_l
              .|. putCCbit eq cc_l
              .|. putCCbit gt cc_l
        excp = NoException -- ????????????????????

----------------------------------------------------------------------
-- Comparison
----------------------------------------------------------------------

compareBinTc :: ArithOp20
compareBinTc cc a b = (cc', excp) where
  bin_lt = a < b
  eq = a == b
  bin_gt = a > b
  a' = tcval a
  b' = tcval b
  tc_lt = a' < b'
  tc_gt = a' > b'
  cc' = putCC bin_lt cci_l $
        putCC eq     cci_E $
        putCC bin_gt cci_g $
        putCC tc_lt  cci_l $
        putCC tc_gt  cci_g $
        0
  excp = NoException

cmpLTWord16, cmpEQWord16, cmpGTWord16 :: Word16 -> Word16 -> Word16

cmpLTWord16 a b = bool16 (tcval a < tcval b)
cmpEQWord16 a b = bool16 (a == b)
cmpGTWord16 a b = bool16 (tcval a > tcval b)


----------------------------------------------------------------------
-- Multiplication
----------------------------------------------------------------------

mul :: ArithOp21
mul cc a b = (cc', x', excp) where
  a' = tcval a
  b' = tcval b
  x = a' * b'
  cc' = cc
  x' = (fromIntegral x) :: Word16
  excp = NoException -- ???????????????
  
{-
mulWord16 :: Word16 -> Word16 -> (Bool, Maybe Word16, Maybe Word16)
mulWord16 a b =
  let
      rightpart16 = a * b :: Word16
      leftpart16 = 0

  in (False, Just rightpart16, Just leftpart16)
mulResult a b sr   =  mulWord16 a b             --  (a*b, 0)
-}

----------------------------------------------------------------------
-- Division
----------------------------------------------------------------------

-- The integer division is intended for cases where all operands and
-- result fit in a 16 bit word with two's complement representation.
-- If the divisor is 0, an exception is produced.  If the programmer
-- intends to check for errors by explicit comparison, this should be
-- done before the division by comparing the b register with 0; there
-- is no zdiv flag in the condition code because the entire R15 is
-- needed to hold the mod.

divInt :: ArithOp21
divInt cc a b = (r, q, excp)
  where q = a `div` b
        r = a `mod` b
        excp = if b == 0 then ArithZeroDivide else NoException

{-
divWord16 :: Word16 -> Word16 -> (Bool, Maybe Word16, Maybe Word16)
divWord16 a b = (False, Just (a `div` b), Just (a `mod` b))

divResult a b sr   =  divWord16 a b             -- (a `div` b, rem)
-}

----------------------------------------------------------------------
-- Logic operations
----------------------------------------------------------------------

-- inv16 :: Word16 -> Word16
-- inv16 = complement

inv16 = undefined
and16 = undefined
or16 = undefined
xor16 = undefined

{-
inv16 :: ArithOp21
inv16 cc a b = (cc', x, NoException) where
  x = complement a
  cc' = cc

and16, or16, xor16 :: ArithOp21
and16 a b = (.&.)
or16  = (.|.)
xor16 = xor
-}

bool16 :: Bool -> Word16
bool16 False = 0
bool16 True = 1

-- Give True iff any bit in a word is 1, give False if all bits are 0.

word16bool :: Word16 -> Bool
word16bool x = x /= 0


invResult a b sr  = (False, Just (inv16 a), Nothing)

andResult a b sr  = (False, Just (and16 a b), Nothing)

orResult a b sr   = (False, Just (or16 a b), Nothing)

xorResult a b sr  = (False, Just (xor16 a b), Nothing)

----------------------------------------------------------------------
-- Shifting and rotation
----------------------------------------------------------------------

-- shift  is from Data.Bits.shift

shiftl16, shiftr16 :: Word16 -> Word16 -> Word16
shiftl16 a b = shiftL a (fromIntegral b)
shiftr16 a b = shiftR a (fromIntegral b)

-- shift :: Word16 -> Word16 -> Word16
-- shift a b = a -- finish ??????

shlResult a b sr  = (False, Just (shiftl16 a b), Nothing)
shrResult a b sr  = (False, Just (shiftr16 a b), Nothing)

----------------------------------------------------------------------
-- Hexadecimal
----------------------------------------------------------------------

-- hexDigits is the list of preferred characters, used for formatting
-- output, while hexChars is the list of characters acceptable for
-- input.

hexChars, hexDigits :: String
hexChars = "0123456789abcdefABCDEF"
hexDigits = "0123456789abcdef"

hexdigit :: Int -> Char
hexdigit i =
  if 0<=i && i<16
    then hexDigits !! i
    else error "hex digit not between 0 and 15"

hexval :: Char -> Int
hexval '0' = 0
hexval '1' = 1
hexval '2' = 2
hexval '3' = 3
hexval '4' = 4
hexval '5' = 5
hexval '6' = 6
hexval '7' = 7
hexval '8' = 8
hexval '9' = 9
hexval 'a' = 10
hexval 'b' = 11
hexval 'c' = 12
hexval 'd' = 13
hexval 'e' = 14
hexval 'f' = 15
hexval 'A' = 10
hexval 'B' = 11
hexval 'C' = 12
hexval 'D' = 13
hexval 'E' = 14
hexval 'F' = 15
hexval _   = 0  -- invalid characters are equivalent to 0

-- Convert (show) a word to hex: word16Hex converts a 16-bit word to a
-- string of four hex digits

word16Hex :: Word16 -> String
word16Hex w =
  let f x i = "0123456789abcdef" !!
        (fromIntegral (Data.Bits.shift x (-i) .&. const16000f))
  in [f w 12, f w 8, f w 4, f w 0]

-- Convert int to hex string

fmthexint :: Int -> Int -> String
fmthexint w n =
  if n<0
    then error "hex number negative"
    else hexloop w n ""

hexloop :: Int -> Int -> String -> String
hexloop w n xs =
  if w==0
    then if n==0
           then xs
           else error "hex number too big"
    else let n' = n `div` 16
             d = hexdigit (n `mod` 16)
         in hexloop (w-1) n' (d:xs)


----------------------------------------------------------------------
-- Converting word to hex strings
----------------------------------------------------------------------

-- Convert a string of bits to a word using binary representation

bitsToWord16 :: String -> Word16
bitsToWord16 bs =
  let
-- pad or trim to 16 bits; argument goes in least significant part
      bs' = take (16 - length bs) (repeat '0') ++ take 16 bs
-- convert characters to numbers
      ds = map (\c -> if c=='1' then 1 else 0) bs'
-- calculate binary representation
      r = sum [x * 2^i | (i,x) <- zip [15,14..0] ds]
  in r


valHex4 :: Integral a => String -> Either String a
valHex4 [p,q,r,s] =
    case (valHexDigit p, valHexDigit q, valHexDigit r, valHexDigit s) of
      (Right p', Right q', Right r', Right s') ->
        Right (16^3 * p' + 16^2 * q' + 16^1 * r' + 16^0 * s')
      _ -> Left ("Bad 4-digit hex constant <" ++ [p,q,r,s] ++ ">")
valHex4 xs = Left ("Bad 4-digit hex constant <" ++ xs ++ ">")

hex4Word16 :: String -> Word16
hex4Word16 x =
  case valHex4 x of
    Right x -> fromIntegral x
    Left _ -> 0

valHexDigit :: Integral a => Char -> Either String a
valHexDigit x =
  if      x=='0' then Right 0
  else if x=='1' then Right 1
  else if x=='2' then Right 2
  else if x=='3' then Right 3
  else if x=='4' then Right 4
  else if x=='5' then Right 5
  else if x=='6' then Right 6
  else if x=='7' then Right 7
  else if x=='8' then Right 8
  else if x=='9' then Right 9
  else if x=='a' then Right 10
  else if x=='A' then Right 10
  else if x=='b' then Right 11
  else if x=='B' then Right 11
  else if x=='c' then Right 12
  else if x=='C' then Right 12
  else if x=='d' then Right 13
  else if x=='D' then Right 13
  else if x=='e' then Right 14
  else if x=='E' then Right 14
  else if x=='f' then Right 15
  else if x=='F' then Right 15
  else Left ("Bad hex digit: " ++ [x])

hex4 :: (Integral a, FiniteBits a) => a -> Char
hex4 x =
  let i =  (fromIntegral x)
  in if i<16
       then "0123456789abcdef" !! i
       else error ("hex digit too large: " ++ show i)

-- CLEANUP hexd and showByte are older versions of hex4, hex8

-- Convert a hex digit represented as Int to the corresponding Char
hexd :: Int -> Char
hexd i
  | 0<=i && i<16 = "0123456789abcdef" !! i
  | otherwise = error "hex digit value not between 0 and 15"

-- Convert a byte, represented as an Int, to a string of hex digits
showByte :: Int -> String
showByte x
  | 0<=x && x<256 =
      let b = x `mod` 16
          a = x `div` 16
      in [hexd a, hexd b]
  | otherwise = error ("Byte value out of range " ++ show x)

hex8 :: Word8 -> String
hex8 x =
  let p = extract 0 3 x
      q = extract 4 7 x
  in [hex4 p, hex4 q]

w16tohex :: Word16 -> Hex4
w16tohex = hex16

hex16 :: Word16 -> String
hex16 x =
  let p = extract  0  3 x
      q = extract  4  7 x
      r = extract  8 11 x
      s = extract 12 15 x
  in [hex4 p, hex4 q, hex4 r, hex4 s]


-- CLEANUP older versions of hex32

showw :: Int -> String
showw x =
  let byted = x `mod` 16
      x1 = x `div` 16
      bytec = x1 `mod` 16
      x2 = x1 `div` 16
      byteb = x2 `mod` 16
      x3 = x2 `div` 16
      bytea = x3
  in [hexd bytea, hexd byteb, hexd bytec, hexd byted]

-- show i using an output string of width w
--showIntWid :: Int -> Int -> String
showIntWid i w =
  let xs = show i :: String
  in (take (w - length xs) (repeat ' ')) ++ xs

hex32 :: Word32 -> String
hex32 x =
  [hex4 (extract  0  3 x),
   hex4 (extract  4  7 x),
   hex4 (extract  8 11 x),
   hex4 (extract 12 15 x),
   ' ',
   hex4 (extract 16 19 x),
   hex4 (extract 20 23 x),
   hex4 (extract 24 27 x),
   hex4 (extract 28 31 x)]

bits4 :: (Integral a, FiniteBits a) => Char -> a
bits4 '0' = 0
bits4 '1' = 1
bits4 '2' = 2
bits4 '3' = 3
bits4 '4' = 4
bits4 '5' = 5
bits4 '6' = 6
bits4 '7' = 7
bits4 '8' = 8
bits4 '9' = 9
bits4 'a' = 10
bits4 'b' = 11
bits4 'c' = 12
bits4 'd' = 13
bits4 'e' = 14
bits4 'f' = 15
bits4 x = error ("Invalid hex digit: " ++ [x])


----------------------------------------------------------------------
-- Converting hex string to word
----------------------------------------------------------------------

{- in PrimData?

type Hex4 = String
zero_hex4 :: Hex4
zero_hex4 = "0000"

bits8 :: String -> Word8
bits8 [a,b] =
  let x = bits4 a :: Word8
      y = bits4 b :: Word8
  in shiftL x 4 .|. y
bits8 _ = 0 -- cover invalid short input


bits16 :: String -> Word16
bits16 [a,b,c,d] =
  let p = bits4 a :: Word16
      q = bits4 b :: Word16
      r = bits4 c :: Word16
      s = bits4 d :: Word16
  in shiftL p 12 .|. shiftL q 8 .|. shiftL r 4 .|. s
bits16 _ = 0 -- cover invalid short input

bits32 :: String -> Word32
bits32 (a:b:c:d:' ':e:f:g:h:_) =
  let p = bits4 a :: Word32
      q = bits4 b :: Word32
      r = bits4 c :: Word32
      s = bits4 d :: Word32
      t = bits4 e :: Word32
      u = bits4 f :: Word32
      v = bits4 g :: Word32
      w = bits4 h :: Word32
  in shiftL p 28 .|. shiftL q 24 .|. shiftL r 20 .|. shiftL s 16 .|.
     shiftL t 12 .|. shiftL u  8 .|. shiftL v  4 .|. w
bits32 _ = 0 -- cover invalid inputs with too few characters

-}

----------------------------------------------------------------------
-- Decimal notation
----------------------------------------------------------------------

fmtdecint :: Int -> Int -> String
fmtdecint w n =
  let xs = show n
  in take (w - length xs) (repeat ' ') ++ xs


-- Formatting Integers

fmtInt :: Int -> Int -> String
fmtInt w i =
  let xs = show i
  in take (w - length xs) (repeat ' ') ++ xs


word16_hex4 :: Word16 -> ([String], Maybe Hex4)
word16_hex4 i = inthex 4 (fromIntegral i)

{- 
 --  ([], Just "abcd")
 if i>= 2^16
    then (["Constant too large: " ++ show i], Nothing)
    else if i<0
           then (["Negatives not supported yet: " ++ show i], Nothing)
           else ([], Just (inthex 4 i))
-}

----------------------------------------------------------------------
-- Constructing and deconstructing words
----------------------------------------------------------------------

bytep, byteq, byter, bytes :: Word32 -> Word8
bytep w = fromIntegral (extract  0  7 w)
byteq w = fromIntegral (extract  8 15 w)
byter w = fromIntegral (extract 16 23 w)
bytes w = fromIntegral (extract 24 31 w)

----------------------------------------------------------------------
-- Output conversions of words
----------------------------------------------------------------------

-- Conversion between integers and hex

-- inthex w i is the w-digit hex representation of i
inthex :: Int -> Int -> ([String], Maybe String)
inthex w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = i `mod` 16
                       b = i `div` 16
                   in g b ++ [hexDigits!!a]
  in if 0 <= i && i<16^w
       then ([], Just (f (g i)))
     else if (-(2^15)) <= i && i <0
       then ([], Just (f (g (2^16 + i))))
     else (["inthex integer out of range " ++ show i], Nothing)
       
-- inthexok allows larger than  w result
--  i is the w-digit hex representation of i

inthexok :: Integral a => Int -> a -> String
inthexok w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = fromIntegral i `mod` 16
                       b = fromIntegral i `div` 16
                   in g b ++ [hexDigits!!a]
  in f (g i)

-- inthex w i converts an Int i to a w-digit hexadecimal
-- representation

{- in PrimData
inthex :: Int -> Int -> ([String], Maybe String)
inthex w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = i `mod` 16
                       b = i `div` 16
                   in g b ++ [hexDigits!!a]
  in if 0 <= i && i<16^w
       then ([], Just (f (g i)))
     else if (-(2^15)) <= i && i <0
       then ([], Just (f (g (2^16 + i))))
     else (["inthex integer out of range " ++ show i], Nothing)

-- inthexok allows larger than w result.  i is the w-digit hex
--  representation of i

inthexok :: Integral a => Int -> a -> String
inthexok w i =
  let f xs = take (w - length xs) (repeat '0') ++ xs
      g i = if i==0
              then []
              else let a = fromIntegral i `mod` 16
                       b = fromIntegral i `div` 16
                   in g b ++ [hexDigits!!a]
  in f (g i)

-}

----------------------------------------------------------------------
-- Converting word to hex strings
----------------------------------------------------------------------


-- CLEANUP hexd and showByte are older versions of hex4, hex8

----------------------------------------------------------------------
-- Converting hex string to word
----------------------------------------------------------------------

type Hex4 = String
zero_hex4 :: Hex4
zero_hex4 = "0000"

bits8 :: String -> Word8
bits8 [a,b] =
  let x = bits4 a :: Word8
      y = bits4 b :: Word8
  in shiftL x 4 .|. y
bits8 _ = 0 -- cover invalid short input

bits16 :: String -> Word16
bits16 [a,b,c,d] =
  let p = bits4 a :: Word16
      q = bits4 b :: Word16
      r = bits4 c :: Word16
      s = bits4 d :: Word16
  in shiftL p 12 .|. shiftL q 8 .|. shiftL r 4 .|. s
bits16 _ = 0 -- cover invalid short input

bits32 :: String -> Word32
bits32 (a:b:c:d:' ':e:f:g:h:_) =
  let p = bits4 a :: Word32
      q = bits4 b :: Word32
      r = bits4 c :: Word32
      s = bits4 d :: Word32
      t = bits4 e :: Word32
      u = bits4 f :: Word32
      v = bits4 g :: Word32
      w = bits4 h :: Word32
  in shiftL p 28 .|. shiftL q 24 .|. shiftL r 20 .|. shiftL s 16 .|.
     shiftL t 12 .|. shiftL u  8 .|. shiftL v  4 .|. w
bits32 _ = 0 -- cover invalid inputs with too few characters

----------------------------------------------------------------------
-- Basic operations on 16-bit machine words
----------------------------------------------------------------------

-- Sigma16 uses 16 bit words.  The fundamental operations are defined
-- here.  It uses binary addition for calculating effective addresses,
-- and two's complement representation for integers.  Note: in the
-- Haskell Word types, such as Word16, the bits are numbered from 0,
-- where bit 0 is the least significant

-- Show the value of a 16-bit word in decimal, assuming it is a binary
-- representation

-- showWord16bin :: Word16 -> String
-- showWord16bin x =
--   let y = (fromIntegral x) :: Word32
--   in show y

showWord16bin :: Word16 -> String
showWord16bin x = show x


-- Show the value of a 16-bit word in decimal, assuming it is a two's
-- complement representation

{-
showWord16tc :: Word16 -> String
showWord16tc x = show (x - 2^15)
-}

showWord16tc :: Word16 -> String
showWord16tc x =
  let s = testBit x 15
      a = if s
            then complement x + 1
            else x
      b = show a
      c = if s then '-' : b else b
  in c


-- mask32: the leftmost 16 bits are 1, the rightmost 16 bits are 0
-- ie it looks like   111...1 000...0

mask32 =
  let x = 0 :: Word32
      y = (complement x) :: Word32
      z = (shiftL y 16) :: Word32
  in z


logic :: Word16 -> Word16 -> Word16
logic a b = a -- finish ???

showHexBinTc x =
  word16Hex x
  ++ "  bin " ++ showWord16bin x
  ++ "  tc "  ++ showWord16tc x
