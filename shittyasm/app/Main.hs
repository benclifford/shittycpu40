module Main where

import Lib
import Numeric (showHex)

data Token = LOAD Int | PUSH | LED Bool | SLEEP Int | DECR Int | JUMPBACKNZ Int | DROP | JUMPZERO
  deriving Show

main :: IO ()
main = shittyasm myprog

shittyasm :: [Token] -> IO ()
shittyasm tokens = do
  let numberedTokens = tokens `zip` [0..]
  putStrLn "// shittyasm start"
  mapM_ emitStatement numberedTokens
  putStrLn "// shittyasm end"

emitStatement :: (Token, Int) -> IO ()
emitStatement (t, n) = putStrLn $ "  ram[" ++ show n ++ "] = " ++ emitToken t ++ ";"

emitToken :: Token -> String
emitToken (LOAD n) = "32'h" ++ showHex v "" where v = 0x60000000 + n
emitToken (SLEEP n) = "32'h" ++ showHex v "" where v = 0x10000000 + n
emitToken (DECR n) = "32'h" ++ showHex v "" where v = 0x40000000 + n
emitToken (JUMPBACKNZ n) = "32'h" ++ showHex v "" where v = 0x50000000 + n
emitToken (LED b) = "32'h" ++ showHex v "" where v = 0x20000000 + n ; n = if b then 1 else 0
emitToken PUSH = "32'h" ++ showHex v "" where v = 0x80000000
emitToken DROP = "32'h" ++ showHex v "" where v = 0x70000000
emitToken JUMPZERO = "32'h" ++ showHex v "" where v = 0x30000000
emitToken t = error $ "non-emittable token " ++ show t

myprog = [
    LOAD 3,
    PUSH,
    LOAD 8,
    LED True,
    SLEEP 0x80000,
    LED False,
    SLEEP 0x80000,
    DECR 1,
    JUMPBACKNZ 5,
    DROP,
    SLEEP 0x800000,
    DECR 1,
    JUMPBACKNZ 11,
    DROP,
    SLEEP 5000000,
    JUMPZERO
  ]

{-
      ram[0] = 32'h6000000A; // load scratch immediate <- 10

      ram[1] = 32'h80000000; // allocate loop variable in scratch
      ram[2] = 32'h60000008; // load scratch immediate <- 8

      // this block eight times
      ram[3] = 32'h20000001; // LED on
      ram[4] = 32'h10080000; // sleep a blip
      ram[5] = 32'h20000000; // LED off
      ram[6] = 32'h10080000; // sleep a blip

      ram[7] = 32'h40000001; // some kind of decrement top of stack by one?

      ram[8] = 32'h50000005; // some kind of jump-back if non-zero / jump if zero - jump back relative, 5

      ram[9] = 32'h70000000; // drop the loop count for this inner loop
      ram[10] = 32'h10800000; // sleep 0.5s

      ram[11] = 32'h40000001; // some kind of decrement top of stack by one?
      ram[12] = 32'h5000000B; // some kind of jump-back if non-zero / jump if zero - jump back relative, 11

      // NO-OP as no stack register file now  ram[6] = 32'h70000000; // some kind of drop from stack, if using stack style registers
 
      ram[13] = 32'h15000000; // sleep 5s
      ram[14] = 32'h30000000; // reset PC to start - jump-unconditional to start

-}
