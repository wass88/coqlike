module Proof where

import Data.List
import Control.Monad

import Term
type Name = String

data Rule = Rule Name [Term] Term
  deriving (Eq, Show, Read)

readRule :: String -> Rule
readRule = undefined

nameOfRule (Rule name _ _) = name
getRule [] name = Nothing
getRule (r@(Rule n _ _):l) name | name == n = Just r
                                | otherwise = getRule l name

data Proof = 
    Goal Term
  | Der Rule [Proof] Term
  deriving (Eq, Read)

instance Show Proof where
  show = show_proof ""

show_proof s (Goal t) = "\n" ++ s ++ "??? " ++ showTerm t ++ ""
show_proof s (Der r ps t) =
  "\n" ++ s ++ showTerm t ++ " by "++ nameOfRule r ++" {" ++
  (intercalate ";" $ map (show_proof ("  "++s)) ps)
  ++ "\n" ++ s ++ "}"

{--
Goal (1 + 2 is 3)
applyRule Hoge 0 ---> Der Hoge [Goal (0 + 2 is 2)] (1 + 2 is 3)
applyRule Piyo 0 ---> Der Hoge [Der Piyo [Goal (2 is 2)] (0 + 2 is 2)] (1 + 2 is 3)
--}

startProof :: Term -> Proof
startProof t = Goal t

endProof :: Proof -> Bool
endProof p = goalList p == []

goalList :: Proof -> [Term]
goalList (Goal t) = [t]
goalList (Der _ ts _) = concatMap goalList ts

apply :: Rule -> Proof -> Either String Proof
apply r p = applyRule r (head $ goalList p) p

applyWith :: Rule -> [(Int, Term)] -> Proof -> Either String Proof
applyWith = undefined

applyRule :: Rule -> Term -> Proof -> Either String Proof
applyRule r k (Goal t) = if Goal k == Goal t
  then do goals <- applyGoals r t; return $ Der r goals t
  else return $ Goal t
applyRule r k (Der rule proofs term) = do
  proofs <- sequence $ fmap (applyRule r k) proofs
  return $ Der rule proofs term

applyGoals :: Rule -> Term -> Either String [Proof] 
applyGoals (Rule name froms to) goal = do
  mp <- termMatch to goal
  return $ map (\c -> Goal (replaceTerm mp c)) froms

checkAssoc :: [(Int, Term)] -> Either String [(Int, Term)]
checkAssoc [] = Right []
checkAssoc ((i,t):l) =
  let (l', s) = span (\(i1, t1) -> i1 == i) l in
  if all (\(_, t1) -> t == t1) l'
  then fmap ((:)(i,t)) $ checkAssoc l
  else Left ("Missmatch "++show i++": "++show t ++" over "++show l)

termMatch :: Term -> Term -> Either String [(Int, Term)]
termMatch t1 t2 = join $ fmap checkAssoc (termMatch_ t1 t2)
  where
  termMatch_ :: Term -> Term -> Either String [(Int, Term)]
  termMatch_ (App name1 ts1) (App name2 ts2)
             | name1 == name2 && length ts1 == length ts2
             = let res = zipWith termMatch_ ts1 ts2 in
             foldl (liftM2 (++)) (Right []) res
  termMatch_ (Var i) t@(App _ _) = Right [(i, t)]
  termMatch_ t1 t2 = Left ("Cannot match: "++show t1++"<->"++show t2)

replaceTerm :: [(Int, Term)] -> Term -> Term
replaceTerm [] t = t
replaceTerm (it:l) t =
  replaceTerm l $ conv it t
  where
    conv :: (Int, Term) -> Term -> Term
    conv (i, t1) (Var j) = (if i == j then t1 else Var j)
    conv it (App name ts) = App name (map (conv it) ts)
