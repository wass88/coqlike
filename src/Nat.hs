module Nat where

import Term
import Proof

fromRight (Right c) = c
fromRight (Left c) = error c

natRules :: [Rule]
natRules = map (\(n, c) ->
  let u = map (fromRight . fromSTerm) c in
    Rule n (tail u) (head u))
  [
    ("P-Zero", ["(is (plus (Z) ($1)) ($1))"]),
    ("P-Succ", ["(is (plus (S($1)) ($2)) (S($0)))",
                "(is (plus ($1) ($2)) ($0))"]),
    ("T-Zero", ["(is (times (Z) ($1)) (Z))"]),
    ("T-Succ", ["(is (times (S($1)) ($2)) ($4))",
                "(is (times ($1) ($2)) ($3))",
                "(is (plus ($2) ($3)) ($4))"])
  ]
pzero  = natRules !! 0
psucc  = natRules !! 1
tzero  = natRules !! 2
tsucc  = natRules !! 3

natTactic (App "is" [(App "plus" [App "Z" [], _]), _]) = Just (pzero, [])
natTactic (App "is" [(App "plus" [App "S" _, _]), _]) = Just (psucc, [])
natTactic (App "is" [(App "times" [App "Z" [], _]), _]) = Just (tzero, [])
natTactic (App "is" [(App "times" [App "S" [t1], t2]), _]) =
  let n1 = numsucc t1 in let n2 = numsucc t2 in
  Just (tsucc, [(3, succnum (n1 * n2))])
natTactic _ = Nothing

natProve :: Proof -> Either String Proof
natProve = applyTactic natTactic