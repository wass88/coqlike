module Arith where

  import Term
  import Proof
  
  import Data.Char (isDigit)
  
  fromRight (Right c) = c
  fromRight (Left c) = error c
  
  prefix = "evalto"
  natRules :: [Rule]
  natRules = map (\(n, c) ->
    let u = map (fromRight . fromSTerm) c in
      Rule n (tail u) (head u))
    [
    ("E-Int", ["("++prefix++" ($1) ($1))"]),
    ("E-Bool", ["("++prefix++" ($1) ($1))"]),
    ("E-IfT", ["("++prefix++" (ite ($1) ($2) ($3)) ($0))","("++prefix++" ($1) (T))","("++prefix++" ($2) ($0))"]),
    ("E-IfF", ["("++prefix++" (ite ($1) ($2) ($3)) ($0))","("++prefix++" ($1) (F))","("++prefix++" ($3) ($0))"]),
    ("E-Plus", ["("++prefix++" (+ ($1) ($2)) ($0))","("++prefix++" ($1) ($3))","("++prefix++" ($2) ($4))","(is (plus ($3) ($4)) ($0))"]),
    ("E-Minus", ["("++prefix++" (- ($1) ($2)) ($0))","("++prefix++" ($1) ($3))","("++prefix++" ($2) ($4))","(is (minus ($3) ($4)) ($0))"]),
    ("E-Times", ["("++prefix++" (* ($1) ($2)) ($0))","("++prefix++" ($1) ($3))","("++prefix++" ($2) ($4))","(is (times ($3) ($4)) ($0))"]),
    ("E-Lt", ["("++prefix++" (< ($1) ($2)) ($0))","("++prefix++" ($1) ($3))","("++prefix++" ($2) ($4))","(is (lt ($3) ($4)) ($0))"]),
    ("B-Plus", ["(is (plus ($1) ($2)) ($3))"]),
    ("B-Minus", ["(is (minus ($1) ($2)) ($3))"]),
    ("B-Times", ["(is (times ($1) ($2)) ($3))"]),
    ("B-Lt", ["(is (lt ($1) ($2)) ($3))"])
    ]
  eint  = natRules !! 0
  ebool  = natRules !! 1
  eift  = natRules !! 2
  eiff  = natRules !! 3
  eplus  = natRules !! 4
  eminus  = natRules !! 5
  etimes  = natRules !! 6
  ebl  = natRules !! 7
  bplus  = natRules !! 8
  bminus  = natRules !! 9
  btimes  = natRules !! 10
  blt  = natRules !! 11
  
  isNum :: [Char] -> Bool
  isNum ('-':t) = isNum' t
  isNum t = isNum' t
  isNum' :: [Char] -> Bool
  isNum' (c:t) = (isDigit c) && (isNum' t)
  isNum' [] = True
  natTactic (App "is" [App c _, _]) | isNum c = Just (eint, [])
  natTactic (App "is" [App "T" _, _]) = Just (ebool, [])
  natTactic (App "is" [App "F" _, _]) = Just (ebool, [])

  natTactic _ = Nothing
  
  natProve :: Proof -> Either String Proof
  natProve = applyTactic natTactic