module Brainfuck where

  import Data.List

  instructions = "><+-[]"
  memory size = fmap (\c -> 0) [1..size]

  type Memory = [Int]
  type Pointer = Int
  data Computer = (Pointer, Memory)

  brainfuck :: Computer -> String -> Computer
  brainfuck _ "" = (p, m)
  brainfuck (p, m) (s:xs) = brainfuck (compute (p, m) s) xs 
    where compute (p', m') s'
      | p' == 0 && s' == '<' = error "Underflow"
      | p' > length m' && s' == '>' = error "Overflow"
      | s' == '<' = (p'-1, m) 
      | s' == '>' = (p'+1, m)
      | s' == '+' = (p', take (p'-1) m ++ [(m' !! p')+1] ++ drop (p'+1) m'
