module Emulator where

import Control.Monad.State
import Control.Concurrent
import Data.Word
import Data.Char
import Data.IORef
import Data.Bits
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Arithmetic
import Architecture
import Common
import State
import Assembler
import Linker

getst = Control.Monad.State.get
putst = Control.Monad.State.put

fetchBlock
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> MemAddress
  -> Int
  -> StateT ArchState UI [Word16]
fetchBlock f a i
  | i <= 0  = return []
  | otherwise = do
      x <- getMem f a
      xs <- fetchBlock f (a+1) (i-1)
      return (x:xs)

storeBlock
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> MemAddress
  -> [Word16]
  -> StateT ArchState UI ()

storeBlock f a [] = return ()
storeBlock f a (x:xs) = do
  setMem f a x
  storeBlock f (a+1) xs

----------------------------------------------------------------------
-- Bootstrap loader
----------------------------------------------------------------------

readLoadModule
  :: (EmuEffect -> StateT ArchState UI ())
  -> MemAddress -> LoadModule -> StateT ArchState UI ()
readLoadModule f a [] =
  do s <- getst
     astsetProcStatus Ready
     return ()
readLoadModule f a (x:xs) =
  do -- liftIO $ putStrLn ("readLoadModule " ++ show a ++ " " ++ show x)
     case x of
       LoadModuleOrg a' -> readLoadModule f a' xs
       LoadModuleData w ->
         do setMem f a w
            readLoadModule f (a+1) xs
       LoadModuleRel w ->   -- not doing relocation yet ???????????
         do setMem f a w
            readLoadModule f (a+1) xs

----------------------------------------------------------------------
-- Run one or more instructions
----------------------------------------------------------------------

-- Check whether the processor is halted; if not, execute an
-- instruction.  Return True iff an instruction was executed

-- CHANGE: Return (astHalted archState); i.e return True if the
-- architecture executed a Halt

executeIfRunning
  :: SystemStateRef
  -> (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> StateT ArchState UI ()
executeIfRunning ssr f g =
  do status <- astgetProcStatus
--     liftIO $ putStrLn ("exIfRunning, status = " ++ show status)
     case status of
       Reset      -> return ()
       Ready      -> executeInstruction f g
       Running    -> executeInstruction f g
       Halted     -> return ()
       Paused     -> executeInstruction f g
       Blocked    -> return ()
       Breakpoint -> executeInstruction f g

-- True -> return True
--       False -> do
-- stillRunning <- executeInstruction f g
--         return stillRunning
--     case astHalted s of
--  s <- getst

-- Run a program on the emulator, in StateT ArchState UI

newStatus
  :: ProcStatus   -- status after executing instruction
  -> Bool         -- breakpoint?
  -> Bool         -- pause?
  -> ProcStatus
newStatus s break pause =
  case break of
    True -> Breakpoint
    False -> case pause of
               True -> Paused
               False -> s

run
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> Maybe BPbool                             -- breakpoint expression
  -> IORef Bool                               -- pause clicked
  -> StateT ArchState UI ()
run f g mbp pauseIORef = do
--  liftIO $ threadDelay 500  -- wait n milliseconds
  executeInstruction f g
  status <- astgetProcStatus
--  liftIO $ putStrLn ("run, status = " ++ show status)
  ast <- getst
  let break = case mbp of
        Nothing -> False
        Just bexp -> evalBPbool bexp ast
  pause <- liftIO $ readIORef pauseIORef
  let status' = newStatus status break pause
--  liftIO $ putStrLn ("(status, status', break, pause) = "
--             ++ show (status, status', break, pause))
  let continue = do
        clearEffectsAst
        run f g mbp pauseIORef
  let stop = return ()
  case status' of
    Reset       -> stop
    Ready       -> continue
    Running     -> continue
    Halted      -> stop
    Paused -> do
      liftIO $ writeIORef pauseIORef False
      stop
    Blocked    -> continue
    Breakpoint -> stop

-- case halted || break || pause of
--    True -> do
--      liftIO $ putStrLn ("run: (halt, break, pause) = "
--                   ++ show (halted, break, pause))
--      liftIO $ putStrLn ("em.run break exp = " ++ show mbp
--                     ++ "\nbreak val = " ++ show break ++
--                     "\n effs = " ++ show (astEffects ast))
--      return ()
--    False -> do
--      clearEffectsAst
--      run f g mbp pauseIORef


clearEffectsAst :: StateT ArchState UI ()
clearEffectsAst = do
  ast <- getst
  putst (ast {astEffects = []})
  return ()

evalBPbool :: BPbool -> ArchState -> Bool
evalBPbool (BPlt x y)  s = evalBPnum x s <  evalBPnum y s
evalBPbool (BPle x y)  s = evalBPnum x s <= evalBPnum y s
evalBPbool (BPeq x y)  s = evalBPnum x s == evalBPnum y s
evalBPbool (BPge x y)  s = evalBPnum x s >= evalBPnum y s
evalBPbool (BPgt x y)  s = evalBPnum x s >  evalBPnum y s
evalBPbool (BPnot x)   s = not (evalBPbool x s)
evalBPbool (BPand x y) s = evalBPbool x s && evalBPbool y s
evalBPbool (BPor x y)  s = evalBPbool x s || evalBPbool y s
evalBPbool (BPxor x y) s =
  let a = evalBPbool x s
      b = evalBPbool y s
  in (a && not b) || (not a && b)
evalBPbool (BPsetMem a) s = f (astEffects s)
  where f [] = False
        f (SetMem ea _ _ : es) =
          if ea==a then True else f es
        f (_ : es) = f es

evalBPnum :: BPnum -> ArchState -> Word16
evalBPnum (BPconst k) s = k
evalBPnum (BPhex xs) s = hex4Word16 xs
evalBPnum BPpc        s = astPC s


{-  old version from summer 2017
run
  :: (EmuEffect -> StateT ArchState IO ())    -- f handles effect
  -> (Instruction -> StateT ArchState IO ())  -- g handles instruction
  -> StateT ArchState IO ()
run f g =
  do running <- executeIfRunning f g
     case running of
       False -> return ()
       True -> run f g
-}


----------------------------------------------------------------------
-- Instruction semantics
----------------------------------------------------------------------

-- Fetch the next instruction and execute it

recordEffect :: EmuEffect -> StateT ArchState UI ()
recordEffect e = do
  s <- getst
  let effs = astEffects s
  let effs' = e : effs
  putst (s {astEffects = effs'})
--  liftIO $ putStrLn ("recordEffect: Just recorded " ++ show e)
  return ()

recordInstruction :: Instruction -> StateT ArchState UI ()
recordInstruction i = do
  s <- getst
  putst (s {astInstruction = i})
  return ()

-- Record the address of the instruction being executed
recordAddrExInstruction :: Word16 -> StateT ArchState UI ()
recordAddrExInstruction a = do
  s <- getst
  putst (s {astAddrExInstr = a})
  return ()


-- execute one instruction; should check first that the processor is
-- in a suitable status

executeInstruction
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> StateT ArchState UI ()
  
executeInstruction f g =
  do incrementICount
     i <- getInstrCount
     s <- getst
--     lift $ astSetInstrCount s (show i)
     pc <- getPC f
     let ia = pc               -- instruction address
     recordAddrExInstruction ia
     ir <- getMem f pc
     setIR f ir
     pc <- return (pc+1)
     setPC f pc
     let (ir_op,ir_d,ir_a,ir_b) = splitword ir
     let rrr20 = runRRR20 f g ia ir_d ir_a ir_b
     let rrr21 = runRRR21 f g ia ir_d ir_a ir_b
     case ir_op of
       0  -> rrr21 Add    addtc
       1  -> rrr21 Sub    subtc
       2  -> rrr21 Mul    mul
       3  -> rrr21 Div    divInt
       4  -> rrr20 Cmp    compareBinTc
       5  -> rrr21 Inv16  addtc  -- temp
       6  -> rrr21 And16  addtc  -- temp
       7  -> rrr21 Or16   addtc  -- temp
       8  -> rrr21 Xor16  addtc  -- temp
       9  -> rrr21 ShiftL addtc  -- temp
       10 -> rrr21 ShiftR addtc  -- temp
       11 -> rrr21 NopRRR addtc  -- temp
       12 -> rrr21 NopRRR addtc  -- temp
       13 -> rrrTrap f g ia ir_d ir_a ir_b
       14 -> rrr21 Add    addtc       -- EX format instruction
       15 ->                          -- RX format instruction
         do ad <- getMem f pc         -- fetch word 2 of instruction
            let disp = ad
            pc <- return (pc+1)       -- increment pc
            setPC f pc
            index <- getRF f ir_a     -- fetch the index register
            ad <- return (ad + index) -- calculate effective address
            setAD f ad
            let rx = reportRX g ia ir_d ir_a ir_b disp ad
            case ir_b of             -- dispatch on secondary opcode
              0  -> do setRF f ir_d ad
                       rx Lea [SetRF ir_d 0 ad]
              1  -> do da <- getMem f ad
                       setRF f ir_d da
                       rx Load [SetRF ir_d 0 da]
              2  -> do d <- getRF f ir_d
                       setMem f ad d
                       rx Store [SetMem ad 0 d]
              3  -> do setPC f ad
                       rx Jump [SetPC 0 ad]
              4  -> do cc <- getRF f 15
                       case selectedCCbit ir_d cc of
                         False -> setPC f ad >> rx Jumpc0 [SetPC 0 ad]
                         True -> rx Jumpc0 [NoJump]
              5  -> do cc <- getRF f 15
                       let temp = selectedCCbit ir_d cc
--                       liftIO $ putStrLn ("FOAFASSF " ++ show temp)
                       case selectedCCbit ir_d cc of
                         False -> do
                           liftIO $ putStrLn "Jumpc1 case False"
                           rx Jumpc1 [NoJump]
                         True -> do
                           liftIO $ putStrLn "Jumpc1 case True"
                           liftIO $ putStrLn (show ad)
                           dummypc1 <- getPC f
                           liftIO $ putStrLn ("pc before jump = "
                                        ++ show dummypc1)
                           setPC f ad
                           rx Jumpc0 [SetPC 0 ad]
                           dummypc2 <- getPC f
                           liftIO $ putStrLn ("final pc = "
                                        ++ show dummypc2)
              6  -> do setRF f ir_d pc
                       setPC f ad
                       rx Jal [SetRF ir_d 0 pc, SetPC 0 ad]
              7  -> do rx NopRX []
              8  -> do rx NopRX []
              9  -> do rx NopRX []
              10 -> do rx NopRX []
              11 -> do rx NopRX []
              12 -> do rx NopRX []
              13 -> do rx NopRX []
              14 -> do rx NopRX []
              15 -> do rx NopRX []
              otherwise -> error "RX ir_b > 15"
       otherwise -> error "ir_op > 15"  -- type can't prevent this
     return ()
--     status <- getProcStatus
--     return status

-- RRR instructions have the form (op rd,ra,rb).  They define the
-- destination register rd as a function of the operand registers ra
-- and rb.  Furthermore, most RRR instructions implicitly set the
-- condition code, which is R15.

-- Trap is a special case, because all three registers provide
-- operands.  Therefore trap is handled separately, by rrr_trap.  Some
-- of the trap codes perform I/O directly, in the emulator, without
-- the use of hardware I/O instructions.

-- (Maybe) A few RRR instructions (multiply and divide) produce two
-- resuts: multiply produces a two-word long product, and divide
-- produces both a quotient and remainder.  The two results are placed
-- in rd and r(d+1).  These cases are handled by rrr_two_results.

-- runRRR20 emulates an RRR instruction with two operands and no
-- ordinary result; the only result is the condition code

runRRR20
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> MemAddress                               -- ia
  -> RegAddress -> RegAddress -> RegAddress   -- ir_d ir_a ir_b
  -> OpSymbol                                 -- symbol for opcode
  -> ArithOp20                                -- semantic function
  -> StateT ArchState UI ()

runRRR20 f g ia rd ra rb op h =
  do a <- getRF f ra
     b <- getRF f rb
     cc <- getRF f 15
     let (cc',excp) = h cc a b
     setRF f 15 cc'
     g (Instruction (RRRinstruction ia rd ra rb) op
          [SetRF 15 cc cc'])
     return ()
-- set excp ????????????????????????????????????????

-- runRRR21 emulates an RRR instruction that takes two operands and
-- produces one result.  It also uses and updates the condition code.

runRRR21
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> MemAddress                               -- ia
  -> RegAddress -> RegAddress -> RegAddress   -- ir_d ir_a ir_b
  -> OpSymbol                                 -- symbol for opcode
  -> ArithOp21                                -- semantic function
  -> StateT ArchState UI ()

runRRR21 f g ia rd ra rb op h =
  do a <- getRF f ra
     b <- getRF f rb
     cc <- getRF f 15
     let (cc',x,excp) = h cc a b
     setRF f 15 cc'
     setRF f rd x
     g (Instruction (RRRinstruction ia rd ra rb) op
          [SetRF 15 cc cc', SetRF rd 0 x])
     return ()
-- ?????????????????????????????????????? set excp

reportRX
  :: (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> MemAddress                               -- ia
  -> RegAddress -> RegAddress -> RegAddress   -- ir_d ir_a ir_b
  -> Word16                                   -- displacement
  -> Word16                                   -- effective address
  -> OpSymbol                                 -- symbol for opcode
  -> [EmuEffect]                              -- primary effects
  -> StateT ArchState UI ()

reportRX g ia rd ra rb disp ea op effects =
  g (Instruction
      (RXinstruction ia rd ra rb disp ea)
       op
       effects)

{-

rrr_2results
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> MemAddress -> RegAddress -> RegAddress
  -> RegAddress -> OpSymbol
  -> StateT ArchState UI ()

rrr_2results f g ia rd ra rb op =
  do
--   liftIO $ putStrLn ("RRR 2 results " ++ show op
--              ++ show (rd,ra,rb))
--     addEffect (Executed (RRRinstruction ia op rd ra rb))
     a <- getRF f ra
     b <- getRF f rb
     let x = 123  -- ??  temp
     setRF f rd x
     return ()
-}


rrrTrap
  :: (EmuEffect -> StateT ArchState UI ())    -- f handles effect
  -> (Instruction -> StateT ArchState UI ())  -- g handles instruction
  -> MemAddress
  -> RegAddress -> RegAddress -> RegAddress
  -> StateT ArchState UI ()

rrrTrap f g ia rd ra rb =
  do d <- getRF f rd
     a <- getRF f ra
     b <- getRF f rb
     case d of
       0 -> -- Halt
         do astsetProcStatus Halted
            f Halt
            g (Instruction (RRRinstruction ia rd ra rb) Trap [Halt])
            return ()
       1 -> -- Read up to b chars into mem[a]...  rb := #chars read
         do s <- getst
-- n = number of characters to read
-- ys = data actually read; remove read input from buf
            liftIO $ putStrLn ("rrrTrap read d=" ++ show d
                       ++ " a=" ++ show a ++ " b=" ++ show b)
            xs <- lift $ astReadOperation s   -- input
            let n = min (length xs) (fromIntegral b)
            let ys = take n xs
            storeBlock f a (map (fromIntegral . ord) ys)
            lift $ astSetInputOperation s (drop n xs)
            liftIO $ putStrLn ("trap read n = " ++ show n)
            setRF f rb (fromIntegral n)
            liftIO $ putStrLn ("read xs = " ++ xs)
            astsetProcStatus Blocked
            return ()
       2 -> -- Write mem[a], ..., mem[a+b-1]
         do s <- getst
            liftIO $ putStrLn ("rrrTrap write code=" ++ show d
                       ++ " a=" ++ show a ++ " b=" ++ show b)
-- restrict character to 8 bits; hope later to allow unicode
            let restrict x = if x>255 then ord '_' else x
            axs <- fetchBlock f a (fromIntegral b)
            let xs = map (toEnum . restrict . fromIntegral) axs
            let ys = xs
--            let ys = "<span class=\"OT\">" ++ xs ++ "</span>"
--            liftIO $ putStrLn ("rrrTrap write data =" ++ ys)
            let oldtext = uiIObuffer s
--            liftIO $ putStrLn ("rrrTrap oldtext = " ++ oldtext)
            let newtext = oldtext ++ ys
--            let newtext = oldtext ++ "<br>" ++ ys
--            liftIO $ putStrLn ("rrrTrap newtext = " ++ newtext)
            let fullnewtext = newtext
--            let fullnewtext = "<html>" ++ newtext ++ "</html>"
--            liftIO $ putStrLn
--               ("rrrTrap fullnewtext = " ++ fullnewtext)
            lift $ astWriteOperation s fullnewtext
--            s <- getst
--            put (s {uiIObuffer = newtext})
            setIObuffer newtext
            g (Instruction (RRRinstruction ia rd ra rb)
                Trap [Write a b])
            astsetProcStatus Blocked
            lift $ flushCallBuffer
            return ()
       otherwise -> -- Ignore other trap requests
         return ()

-- This is an alternative way to get op:
--     let opCode = decodeOpSymbol ir_op ir_d ir_a ir_b

----------------------------------------------------------------------
-- Accessing registers and memory
----------------------------------------------------------------------

incrementICount :: StateT ArchState UI ()
incrementICount = do
  s <- getst
  let i = astInstrCount s
  putst (s {astInstrCount = i+1})

-- Set the I/O buffer text
setIObuffer :: String -> StateT ArchState UI ()
setIObuffer xs = do
  s <- getst
  put (s {uiIObuffer = xs})
  return ()

-- Think about this: really have old val in effect?

-- The access functions provide a hook which is executed when a
-- register or memory location is read or written, enabling the user
-- interface to indicate what values have been accessed.

getInstrCount :: StateT ArchState UI Int
getInstrCount = do
  s <- getst
  let i = astInstrCount s
  return i

getPC :: (EmuEffect -> StateT ArchState UI ())
  -> StateT ArchState UI Word16
getPC f =
  do s <- getst
     let pc = astPC s
     f (GetPC pc)
     return pc

setPC :: (EmuEffect -> StateT ArchState UI ())
  -> Word16 -> StateT ArchState UI ()
setPC f x =
  do s <- getst
     putst (s {astPC = x})
     f (SetPC (astPC s) x)
     return ()

getIR :: (EmuEffect -> StateT ArchState UI ())
  -> StateT ArchState UI Word16
getIR f =
  do s <- getst
     let ir = astIR s
     f (GetIR ir)
     return ir

setIR :: (EmuEffect -> StateT ArchState UI ())
  -> Word16 -> StateT ArchState UI ()
setIR f x =
  do s <- getst
     putst (s {astIR = x})
     f (SetIR (astIR s) x)
     return ()

getAD ::  (EmuEffect -> StateT ArchState UI ())
  -> StateT ArchState UI Word16
getAD f =
  do s <- getst
     let ad = astAD s
     f (GetAD ad)
     return ad

setAD :: (EmuEffect -> StateT ArchState UI ())
  -> Word16 -> StateT ArchState UI ()
setAD f x =
  do s <- getst
     putst (s {astAD = x})
     f (SetAD (astAD s) x)
     return ()

getDAT :: (EmuEffect -> StateT ArchState UI ())
  -> StateT ArchState UI Word16
getDAT f =
  do s <- getst
     let pc = astDAT s
     f (GetDAT pc)
     return pc

setDAT :: (EmuEffect -> StateT ArchState UI ())
  -> Word16 -> StateT ArchState UI ()
setDAT f x =
  do s <- getst
     putst (s {astDAT = x})
     f (SetDAT (astDAT s) x)
     return ()

getRF :: (EmuEffect -> StateT ArchState UI ())
  -> RegAddress -> StateT ArchState UI Word16
getRF f a =
  do s <- getst
     let rf = astRF s
     let x = fetchRF rf a
     f (GetRF a x)
     return x

setRF :: (EmuEffect -> StateT ArchState UI ())
  -> RegAddress -> Word16 -> StateT ArchState UI ()
setRF f a x =
  do s <- getst
     let rf = astRF s
     let (y,rf') = loadRF rf a x
     putst (s {astRF = rf'})
     f (SetRF a y x)
     return ()

getMem :: (EmuEffect -> StateT ArchState UI ())
  -> MemAddress -> StateT ArchState UI Word16
getMem f a =
  do s <- getst
     let mem = astMem s
     let x = fetchMem mem a
     f (GetMem a x)
     return x

setMem :: (EmuEffect -> StateT ArchState UI ())
  -> MemAddress -> Word16 -> StateT ArchState UI ()
setMem f a x =
  do s <- getst
     let mem = astMem s
     let (y,mem') = storeMem mem a x
     putst (s {astMem = mem'})
     f (SetMem a y x)
     return ()

-- Set value in the memory displays




----------------------------------------------------------------------
-- Effects
----------------------------------------------------------------------

ignoreEffect :: EmuEffect -> StateT ArchState UI ()
ignoreEffect e = return ()
ignoreInstruction :: Instruction -> StateT ArchState UI ()
ignoreInstruction i = return ()

-- Each effect is handled by updating the CtlState, which can be
-- displayed by the user interface

handleEffects :: [EmuEffect] -> StateT ArchState UI ()
handleEffects [] = return ()
handleEffects (e:es) = handleEffect e >> handleEffects es

handleEffect :: EmuEffect -> StateT ArchState UI ()
handleEffect (GetPC pc) =
  do s <- getst
--     let (oldpc,oldused,oldmod) = ui_pc s
--     put (s {ui_pc = (pc, True, oldmod)} )
     return ()
handleEffect (SetPC oldpc newpc) =
  do s <- getst
--     let (oldpc,oldused,oldmod) = ui_pc s
--     put (s {ui_pc = (newpc, oldused, True)} )
     return ()
handleEffect (GetIR ir) =
  do s <- getst
--     let (oldir,oldused,oldmod) = ui_ir s
--     put (s {ui_ir = (ir, True, oldmod)} )
     return ()
handleEffect (SetIR oldir newir) =
  do s <- getst
--     let (oldir,oldused,oldmod) = ui_ir s
--     put (s {ui_ir = (newir, oldused, True)} )
     return ()
handleEffect (GetAD ad) =
  do s <- getst
--     let (oldad,oldused,oldmod) = ui_ad s
--     put (s {ui_ad = (ad, True, oldmod)} )
     return ()
handleEffect (SetAD oldad newad) =
  do s <- getst
--     let (oldad,oldused,oldmod) = ui_ad s
--     put (s {ui_ad = (newad, oldused, True)} )
     return ()
handleEffect (GetRF r x) =
  do s <- getst
--     let rfEffects = ui_rf s
--     put (s {ui_rf = (r,x,True,False) : rfEffects})
     return ()
handleEffect (SetRF r old new) =
  do s <- getst
--     let rfEffects = ui_rf s
--     put (s {ui_rf = (r,new,True,False) : rfEffects})
     return ()
handleEffect (GetMem a x) =
  do s <- getst
--     let memEffects = ui_mem s
--     put (s {ui_mem = (a,x,True,False) : memEffects})
     return ()
handleEffect (SetMem a old new) =
  do s <- getst
--     let memEffects = ui_mem s
--     put (s {ui_mem = (a,new,False,True) : memEffects})
     return ()
-- handleEffect (Executed i) =
--   do s <- getst
--     put (s {ui_instr = i})
--     return ()



