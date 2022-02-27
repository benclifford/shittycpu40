{-# Language RecursiveDo #-}

module Main where

import Lib
import Control.Monad.State.Lazy
import Data.Char
import Numeric (showHex)

data Token = LOAD Int | LED | SLEEP | DECR Int | JUMPBACKNZ Int | DROP | RET | GOSUB Int | CONSOLEUARTINIT | CONSOLEWRITESTACK | CONSOLEREAD | TONEGEN
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
  i $ JUMPBACKNZ rel

console_print_str :: String -> State ResolverState ()
console_print_str msg = 
  forM_ msg $ \c -> do
    i $ LOAD (ord c)
    i $ CONSOLEWRITESTACK 

codify :: Token -> Int
codify (LOAD n) = 0x60000000 + n
codify (DECR n) = 0x40000000 + n
codify (JUMPBACKNZ n) = 0x50000000 + n
codify (GOSUB n) = 0x90000000 + n
codify DROP = 0x70000000
codify RET = 0xA0000000
codify CONSOLEUARTINIT = 0xB1000000
codify (CONSOLEREAD) = 0xB3000000
codify (CONSOLEWRITESTACK) = 0xB4000000
codify TONEGEN = 0xC0000000
codify SLEEP = 0xC1000000
codify LED = 0xC2000000
codify t = error $ "non-emittable token " ++ show t

myprog = myprog_loop_flash

myprog_inter :: State ResolverState ()
myprog_inter = mdo
    i $ CONSOLEUARTINIT
    i $ GOSUB print_banner
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
    print_banner <- here
    console_print_str "ShittyFirmware40/interactive -- Ben Clifford, benc@hawaga.org.uk\n"
    i $ RET


define_interact_inner = mkSubroutine $ do
    -- console_print_str "u"
    -- i $ LOAD 118
    -- i $ CONSOLEWRITESTACK

    -- console_print_str "w"
    -- console_print_str "r"
    i $ CONSOLEREAD
    -- console_print_str "p1"
    -- console_print_str "q1"
    i $ CONSOLEWRITESTACK
    -- console_print_str "s"
    -- i $ SLEEP 0x1000000
    -- console_print_str "\n"
    i $ RET

mkSubroutine code = do
    start <- here
    code
    return start

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

