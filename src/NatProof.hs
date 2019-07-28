module NatProof where

import Term as T
import Proof as P


myRules :: [P.Rule]
myRules = map (\(n, c) ->
  let u = map (fromRight . T.fromSTerm) c in
    P.Rule n (tail u) (head u))
  [
    ("P-Zero", ["(is (plus (Z) ($0)) ($0))"]),
    ("P-Succ", ["(is (plus (S ($1)) ($2)) (S ($0)))", "(is (plus ($1) ($2)) ($0))"])
  ]
-- P.Rule "P-Zero" [] (T.App "is" [T.App "plus" [T.App "Z" [], T.Var 0], T.Var 0])

-- myGoal = P.startProof $ T.App "is" [T.App "plus" [T.App "Z" [], T.App "S" []], T.App "S" []]
fromRight (Right c) = c
fromRight (Left c) = error c

-- goal = "(is (plus (Z) (S)) (S))"
goal = "(is (plus (S(Z)) (S(Z))) (S (S (Z))))"
myGoal = P.startProof $ fromRight $ T.fromSTerm goal

nxGoal0 = fromRight $ P.apply (myRules !! 1) myGoal
nxGoal = P.apply (myRules !! 0) nxGoal0

