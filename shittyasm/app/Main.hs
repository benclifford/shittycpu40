{-# Language RecursiveDo #-}

module Main where

import Lib
import Control.Monad.State.Lazy
import Data.Char
import Numeric (showHex)

data Token = LOAD Int | LED | SLEEP | ADD | DECR Int | JUMPBACKNZ Int | DROP | DUP | OVER | PUSH | POP | RET | GOSUB Int | CONSOLEUARTINIT | CONSOLEWRITESTACK | CONSOLEREAD | TONEGEN
  deriving Show

main :: IO ()
main = shittyasm myprog

shittyasm :: State ResolverState () -> IO ()
shittyasm prog = do
  let tokens = resolveProg prog
  let numberedTokens = tokens `zip` [0..]
  putStrLn "// shittyasm start"
  mapM_ emitStatement numberedTokens
  putStrLn "// shittyasm end"

emitStatement :: (Token, Int) -> IO ()
emitStatement (t, n) = putStrLn $ "  ram[" ++ show n ++ "] = " ++ (emit . codify) t ++ ";"

emit :: Int -> String
emit v = "32'h" ++ showHex v ""

type ResolverState = ([Token])

resolveProg :: State ResolverState () -> [Token]
resolveProg prog = execState prog []

i :: Token -> State ResolverState ()
i token = do
  s <- get
  put (s ++ [token])

here :: State ResolverState Int
here = do
  s <- get
  return (length s)

jumpbacknz_absolute :: Int -> State ResolverState ()
jumpbacknz_absolute target_abs_addr = do
  instr_abs_addr <- here
  let rel = instr_abs_addr - target_abs_addr
  when (rel < 0) $ error "Relative jump value is negative for JUMPBACKNZ - i.e. target is forwards, not backwards"
  i $ JUMPBACKNZ rel

console_print_str :: String -> State ResolverState ()
console_print_str msg = 
  forM_ msg $ \c -> do
    i $ LOAD (ord c)
    i $ CONSOLEWRITESTACK 

codify :: Token -> Int
codify (LOAD n) = 0x60000000 + n
codify ADD = 0x80000000
codify (DECR n) = 0x40000000 + n
codify (JUMPBACKNZ n) = 0x50000000 + n
codify (GOSUB n) = 0x90000000 + n
codify DROP = 0x70000000
codify DUP = 0x71000000
codify OVER = 0x72000000
codify PUSH = 0x78000000
codify POP = 0x79000000
codify RET = 0xA0000000
codify CONSOLEUARTINIT = 0xB1000000
codify (CONSOLEREAD) = 0xB3000000
codify (CONSOLEWRITESTACK) = 0xB4000000
codify TONEGEN = 0xC0000000
codify SLEEP = 0xC1000000
codify LED = 0xC2000000
codify t = error $ "non-emittable token " ++ show t

myprog = myprog_menu

myprog_menu :: State ResolverState ()
myprog_menu = mdo
    i $ CONSOLEUARTINIT
    i $ GOSUB print_banner
    console_print_str "OVER test: should see ABA: "
    i $ LOAD (ord 'A')
    i $ LOAD (ord 'B')
    i $ OVER
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK
    console_print_str "\n"
 
    console_print_str "PUSH/POP test - 2 values: should see CD: "
    i $ LOAD (ord 'D')
    i $ LOAD (ord 'C')
    i $ PUSH
    i $ PUSH
    i $ POP
    i $ POP
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK
    console_print_str "\n"

   
    console_print_str "PUSH/POP test - 3 values: should see FGH: "
    i $ LOAD (ord 'H')
    i $ LOAD (ord 'G')
    i $ LOAD (ord 'F')
    i $ PUSH
    i $ PUSH
    i $ PUSH
    i $ POP
    i $ POP
    i $ POP
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK
    console_print_str "\n"


    console_print_str "Swap test: should see AB: "

    i $ LOAD (ord 'A')
    i $ LOAD (ord 'B')
    swap
    i $ CONSOLEWRITESTACK
    i $ CONSOLEWRITESTACK

    console_print_str "\n"

    console_print_str "Waiting for one char: "
    i $ GOSUB read_char  -- reads a char, waiting for a valid one rather than returning -1
    i $ CONSOLEWRITESTACK

    console_print_str "Entering interactive loop."

    myprog_inter

    print_banner <- here
    console_print_str "ShittyFirmware40/interactive r13 -- Ben Clifford, benc@hawaga.org.uk\n"
    i $ RET

    read_char <- here

    i $ CONSOLEREAD

    goto valid_test

    valid_true <- here
    i $ DROP  -- drop the test condition
    -- now the stack is, on top, the read char, and below it,
    -- the return address.
    -- need to RET to the return address, leaving the read char
    -- on the stack.
    -- we could use SWAP here... but charles moore says I can do that
    -- with other more fundamental (but more complicated operators)
    swap
    i $ RET

    valid_test <- here
    i $ DUP
    i $ LOAD 1
    i $ ADD
    jumpbacknz_absolute valid_true
    i $ DROP  -- drop the test condition
    i $ DROP  -- drop the character instead of writing it
    goto read_char

    write_continue <- here
 
    i $ RET

myprog_inter :: State ResolverState ()
myprog_inter = mdo
    outer_loop <- here
    i $ GOSUB interact_inner
    goto outer_loop

    -- This needs importing in just once.
    -- It's safe to import multiple times but wasteful of
    -- space. Is there a way to implement this single-import
    -- in an easy way? mdo makes it easy to have locally
    -- scoped symbols... but how do I do optional-global
    -- symbols?
    console_print_str "$ "
    interact_inner <- define_interact_inner
    return ()


define_interact_inner = mkSubroutine $ mdo
    -- console_print_str "u"
    -- i $ LOAD 118
    -- i $ CONSOLEWRITESTACK

    -- console_print_str "w"
    -- console_print_str "r"
    i $ CONSOLEREAD
    -- console_print_str "p1"
    -- console_print_str "q1"

    -- This write should only happen if the read is not a "no-data" value
    -- which is (according to the picosoc docs, all 1s)

    goto write_test

    write_body <- here
    i $ DROP -- drop the test condition
    i $ CONSOLEWRITESTACK

    goto write_continue

    write_test <- here

    -- TODO: we need to test if the top of the stack
    -- is all 1s...

    -- what we have available is a test that checks
    -- if it is 0, without dropping.

    -- so... make a new stack value that is the character + 1, assuming rollover
    -- then test against that - it will be 0 when the value is -1
    -- then in both routes, drop that duplicated value
    -- because JUMPBACKNZ does not do that.
    -- so I need two new instructions:
    -- DUP
    -- ADD (impl but not tested)

    i $ DUP
    i $ LOAD 1
    i $ ADD

    jumpbacknz_absolute write_body
    -- ... else ... 
    i $ DROP  -- drop the test condition
    i $ DROP  -- drop the character instead of writing it
    goto write_continue -- doesn't need to happen if continue is right after... but non-fallthrough appeals to my taste in modularity

    write_continue <- here

    -- console_print_str "s"
    -- i $ SLEEP 0x1000000
    -- console_print_str "\n"
    i $ RET

mkSubroutine code = do
    start <- here
    code
    return start

-- a b    a b a     a b | a    a | b a    | b a    b | a     b a |
swap :: State ResolverState ()
swap = do
    i $ OVER
    i $ PUSH
    i $ PUSH
    i $ DROP
    i $ POP
    i $ POP


goto :: Int -> State ResolverState ()
goto a = do
    i $ LOAD a
    i $ RET

myprog_loop_flash :: State ResolverState ()
myprog_loop_flash = mdo

    i $ CONSOLEUARTINIT
    i $ GOSUB print_banner
    i $ GOSUB initbeeps
    i $ LOAD 0
    i $ TONEGEN

    restart <- here
    i $ LOAD 3

    outer_loop <- here
    console_print_str "Outer loop body start.\n"
    i $ LOAD 5

    middle_loop <- here
    i $ LOAD 20

    inner_loop <- here
    i $ GOSUB pulse_leds
    i $ DECR 1
    jumpbacknz_absolute inner_loop

    i $ DROP
    i $ LOAD 0x1000000
    i $ SLEEP
    i $ DECR 1
    jumpbacknz_absolute middle_loop

    i $ DROP
    i $ LOAD 1
    i $ LED
    i $ LOAD 0x1000000
    i $ SLEEP
    i $ LOAD 0
    i $ LED
    i $ DECR 1
    jumpbacknz_absolute outer_loop

    i $ DROP
    console_print_str "Outer loop completed.\n"
    i $ LOAD 0x5000000
    i $ SLEEP
    i $ LOAD restart
    i $ RET -- RET >> LOAD = GOTO

    pulse_leds <- here
    i $ LOAD 1
    i $ LED
    i $ LOAD 0x80000
    i $ SLEEP
    i $ LOAD 0
    i $ LED
    i $ LOAD 0x80000
    i $ SLEEP
    i $ RET

    initbeeps <- here
    i $ LOAD 3
    initbeeps_loop <- here
    i $ LOAD 0x4000
    i $ TONEGEN
    i $ LOAD 1600000
    i $ SLEEP
    i $ LOAD 0
    i $ TONEGEN
    i $ LOAD 14400000
    i $ SLEEP
    i $ DECR 1
    jumpbacknz_absolute initbeeps_loop
    i $ DROP
    i $ LOAD 0x4000
    i $ TONEGEN
    i $ LOAD 8000000
    i $ SLEEP
    i $ LOAD 0
    i $ TONEGEN
    i $ RET

    print_banner <- here
    console_print_str "ShittyFirmware40 -- Ben Clifford, benc@hawaga.org.uk\n"
    i $ RET

