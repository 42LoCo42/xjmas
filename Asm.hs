module Asm where

import XJmas

asm_list = zip ["umc", "take", "save",
           "add", "sub", "test",
           "jump", "inc", "dec",
           "null", "stop", "iomap"] [1..]

first_addr = (length asm_list+1) * 2

safeHead :: a -> [a] -> a
safeHead a []    = a
safeHead _ (h:_) = h

zipWithAddr :: Int -> [Line] -> [(Line, Int)]
zipWithAddr _ []            = []
zipWithAddr i (h@(Txt _):t) = (h, 0) : zipWithAddr i t
zipWithAddr i (h:t)         = (h, i) : zipWithAddr (i+1) t

assemble :: [(Line, Int)] -> (Line, Int) -> String
assemble _ ((Txt s),_)          = s
assemble _ ((Raw _ val),_)      = show val
assemble _ ((Asm _ "umc" [
  Num a,
  Num b,
  Num c]),_) = (show $ 1000 + a) ++ "\n" ++ (show $ 1000*b + c)

assemble p ((Asm _ asm [arg]), addr) = show $ (1000*a + b)
  where
    a = snd $ head $ filter (\(s,_) -> s == asm) asm_list
    b = first_addr + asmArg p addr arg
assemble _ other = "# " ++ show other

asmArg :: [(Line, Int)] -> Int -> Arg -> Int
asmArg _ _    (Num n)   = n
asmArg p addr (Label l) = base + offset
  where
    base   = if stringOf l == "" then addr else
                snd $ safeHead (Txt "", -99999) $
                filter (\(line, _) -> hasLabel l line) p
    offset = offsetOf l

hasLabel :: Label -> Line -> Bool
hasLabel _ (Txt _)     = False
hasLabel s (Raw l _)   = maybe False (== stringOf s) l
hasLabel s (Asm l _ _) = maybe False (== stringOf s) l

stringOf :: Label -> String
stringOf (Direct s)   = s
stringOf (Offset s _) = s

offsetOf :: Label -> Int
offsetOf (Direct _)   = 0
offsetOf (Offset _ i) = i
