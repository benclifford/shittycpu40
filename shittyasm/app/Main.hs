{-# Language RecursiveDo #-}

module Main where

import Lib
import Control.Monad.State.Lazy
import Data.Char
import Numeric (showHex)

data Token = LOAD Int | LED Bool | SLEEP Int | DECR Int | JUMPBACKNZ Int | DROP | RET | GOSUB Int | CONSOLEUARTINIT | CONSOLEWRITE Int | TONEGEN Int
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
  forM_ msg (i . CONSOLEWRITE . ord)

codify :: Token -> Int
codify (LOAD n) = 0x60000000 + n
codify (SLEEP n) = 0x10000000 + n
codify (DECR n) = 0x40000000 + n
codify (JUMPBACKNZ n) = 0x50000000 + n
codify (GOSUB n) = 0x90000000 + n
codify (LED b) = 0x20000000 + n where n = if b then 1 else 0
codify DROP = 0x70000000
codify RET = 0xA0000000
codify CONSOLEUARTINIT = 0xB1000000
codify (CONSOLEWRITE n) = 0xB2000000 + n
codify (TONEGEN n) = 0xC0000000 + n
codify t = error $ "non-emittable token " ++ show t

myprog :: State ResolverState ()
myprog = mdo

    i $ CONSOLEUARTINIT
    i $ GOSUB print_banner
    i $ GOSUB initbeeps
    i $ TONEGEN 0

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
    i $ SLEEP 0x1000000
    i $ DECR 1
    jumpbacknz_absolute middle_loop

    i $ DROP
    i $ LED True
    i $ SLEEP 0x1000000
    i $ LED False
    i $ DECR 1
    jumpbacknz_absolute outer_loop

    i $ DROP
    console_print_str "Outer loop completed.\n"
    i $ SLEEP 0x5000000
    i $ LOAD restart
    i $ RET -- RET >> LOAD = GOTO

    pulse_leds <- here
    i $ LED True
    i $ SLEEP 0x80000
    i $ LED False
    i $ SLEEP 0x80000
    i $ RET

    initbeeps <- here
    i $ LOAD 3
    initbeeps_loop <- here
    i $ TONEGEN 0x4000
    i $ SLEEP 1600000
    i $ TONEGEN 0
    i $ SLEEP 14400000
    i $ DECR 1
    jumpbacknz_absolute initbeeps_loop
    i $ DROP
    i $ TONEGEN 0x4000
    i $ SLEEP 8000000
    i $ TONEGEN 0
    i $ RET

    print_banner <- here
    console_print_str "ShittyFirmware40 -- Ben Clifford, benc@hawaga.org.uk\n"
    i $ RET

