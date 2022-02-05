module Main where

import Lib
import Numeric (showHex)

data Token = LOAD Int | LED Bool | SLEEP Int | DECR Int | JUMPBACKNZ Int | DROP | RET | GOSUB Int | CONSOLEUARTINIT | CONSOLEWRITE Int | TONEGEN Int
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
emitStatement (t, n) = putStrLn $ "  ram[" ++ show n ++ "] = " ++ (emit . codify) t ++ ";"

emit :: Int -> String
emit v = "32'h" ++ showHex v ""

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

myprog = [
    CONSOLEUARTINIT,
    GOSUB 30,
    TONEGEN 0,
    LOAD 3,
    CONSOLEWRITE 83, -- 83 is the code for Y - could write haskell compile time code for this...
    LOAD 5,
    LOAD 20,
    GOSUB 25,
    DECR 1,
    JUMPBACKNZ 2,
    DROP,
    SLEEP 0x1000000,
    DECR 1,
    JUMPBACKNZ 7,
    DROP,
    LED True,
    SLEEP 0x1000000,
    LED False,
    DECR 1,
    JUMPBACKNZ 15,
    DROP,
    CONSOLEWRITE 82, -- 82 is the code for R - could write haskell compile time code for this...
    SLEEP 0x5000000,
    LOAD 3,
    RET, -- RET >> LOAD = GOTO
    LED True,
    SLEEP 0x80000,
    LED False,
    SLEEP 0x80000,
    RET,
    LOAD 3,
    TONEGEN 0x4000,
    SLEEP 1600000,
    TONEGEN 00,
    SLEEP 14400000,
    DECR 1,
    JUMPBACKNZ 5,
    DROP,
    TONEGEN 0x4000,
    SLEEP 8000000,
    TONEGEN 0,
    RET
  ]

