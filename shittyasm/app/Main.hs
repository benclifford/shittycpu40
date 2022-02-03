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
    PUSH,
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
    SLEEP 0x5000000,
    JUMPZERO
  ]
