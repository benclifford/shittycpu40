module Main where

import Lib
import Numeric (showHex)

data Token = LOAD Int | PUSH | LED Bool | SLEEP Int | DECR Int | JUMPBACKNZ Int | DROP | JUMPZERO | RET | GOSUB Int | CONSOLEUARTINIT | CONSOLEWRITE Int
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
emitToken (GOSUB n) = "32'h" ++ showHex v "" where v = 0x90000000 + n
emitToken (LED b) = "32'h" ++ showHex v "" where v = 0x20000000 + n ; n = if b then 1 else 0
emitToken PUSH = "32'h" ++ showHex v "" where v = 0x80000000
emitToken DROP = "32'h" ++ showHex v "" where v = 0x70000000
emitToken JUMPZERO = "32'h" ++ showHex v "" where v = 0x30000000
emitToken RET = "32'h" ++ showHex v "" where v = 0xA0000000
emitToken CONSOLEUARTINIT = "32'h" ++ showHex v "" where v = 0xB1000000
emitToken (CONSOLEWRITE n) = "32'h" ++ showHex v "" where v = 0xB2000000 + n
emitToken t = error $ "non-emittable token " ++ show t

myprog = [
    CONSOLEUARTINIT,
    PUSH,
    LOAD 3,
    CONSOLEWRITE 83, -- 83 is the code for Y - could write haskell compile time code for this...
    PUSH,
    LOAD 5,
    PUSH,
    LOAD 20,
    GOSUB 25,
    DECR 1,
    JUMPBACKNZ 2,
    -- GOSUB 18,  -- if I move this gosub up one line to immediately before the drop, I get the inner loop repeated blipblip-long, without ever getting the final delay. But I do get the delay that happens right after this gosub, by the looks of things? (visually)
    -- ... which makes it seem like the outer loop (that should stop after 3 steps) is never ending. so some kind of pushing/popping of values into ram that happens because of the gosub/ret/drop interaction is going awry. The inner loop pointer is being restored OK, I think, when the GOSUB returns -- otherwise the outer loop would be broken?
    -- and the general PUSH/DROP mechanism is working ok for one level of stuff going to the stack. So maybe the bug is when the ram-stack becomes 2 cells deep? i.e. x,y,ret on the stack. the other situations are only x,ret and x,y. which would suggest something awry with the stack pointer changing, rather than with the values in ram? In which case, a triple nested for loop would also break, with no GOSUB involved at all?
    DROP,
    SLEEP 0x1000000,
    DECR 1,
    JUMPBACKNZ 8,
    DROP,
    LED True,
    SLEEP 0x1000000,
    LED False,
    DECR 1,
    JUMPBACKNZ 17,
    DROP,
    CONSOLEWRITE 82, -- 82 is the code for R - could write haskell compile time code for this...
    SLEEP 0x5000000,
    JUMPZERO,
    LED True,
    SLEEP 0x80000,
    LED False,
    SLEEP 0x80000,
    RET
  ]

old_myprog = [
    PUSH, -- alloc x
    LOAD 3,  -- x
    PUSH, -- alloc y
    LOAD 8, -- load y
    LED True,
    SLEEP 0x80000,
    LED False,
    SLEEP 0x80000,
    DECR 1,
    JUMPBACKNZ 5,
    DROP, -- drops y
    SLEEP 0x800000,
    DECR 1,
    JUMPBACKNZ 13,
    DROP, -- drops x
    SLEEP 0x5000000,
    JUMPZERO
  ]

