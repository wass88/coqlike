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

currentGoal :: Proof -> Maybe Term
currentGoal p = case goalList p of [] -> Nothing; h:_ -> Just h

apply :: Rule -> Proof -> Either String Proof
apply r = applyWith r []

applyWith :: Rule -> [(Int, Term)] -> Proof -> Either String Proof
applyWith r b p = applyRuleWith r b (head $ goalList p) p

applyRuleWith :: Rule -> [(Int, Term)] -> Term -> Proof -> Either String Proof
applyRuleWith r b k (Goal t) = if Goal k == Goal t
  then do goals <- applyGoals r b t; return $ Der r goals t
  else return $ Goal t
applyRuleWith r b k (Der rule proofs term) = do
  proofs <- sequence $ fmap (applyRuleWith r b k) proofs
  return $ Der rule proofs term

applyRule :: Rule -> Term -> Proof -> Either String Proof
applyRule r = applyRuleWith r []

applyGoals :: Rule -> [(Int, Term)] -> Term -> Either String [Proof] 
applyGoals (Rule name froms to) b goal = do
  mp <- termMatch b to goal
  return $ map (\c -> Goal (replaceTerm mp c)) froms

checkAssoc :: [(Int, Term)] -> Either String [(Int, Term)]
checkAssoc [] = Right []
checkAssoc ((i,t):l) =
  let (l', s) = span (\(i1, t1) -> i1 == i) l in
  if all (\(_, t1) -> t == t1) l'
  then fmap ((:)(i,t)) $ checkAssoc l
  else Left ("Missmatch "++show i++": "++show t ++" over "++show l)

termMatch :: [(Int, Term)] -> Term -> Term -> Either String [(Int, Term)]
termMatch b t1 t2 = join $ fmap checkAssoc ((fmap (b ++)) (termMatch_ t1 t2))
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

type TacTic = Term -> Maybe (Rule, [(Int, Term)])
applyTactic :: TacTic -> Proof -> Either String Proof
applyTactic tt p = 
  case currentGoal p of
    Nothing -> Right p
    Just g ->
      case tt g of 
        Nothing -> Right p
        Just (rule, b) -> do
          p <- applyWith rule b p 
          applyTactic tt p
