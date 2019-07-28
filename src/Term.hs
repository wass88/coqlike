module Term where

import Text.Parsec
import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.List

type Token = String

data Term =
    Var Int
  | App Token [Term]
  deriving (Eq, Show, Read)

showTerm (Var i) = "$" ++ show i
showTerm (App n ts) = show_term n ts
  where
  s = showTerm
  show_term "is" [t1,t2] = s t1 ++ " is " ++s t2
  show_term "plus" [t1,t2] = s t1 ++ " plus " ++s t2
  show_term "minus" [t1,t2] = s t1 ++ " minus " ++ s t2
  show_term "times" [t1,t2] = s t1 ++ " times " ++ s t2
  show_term "lt" [t1,t2] = s t1 ++ " less than " ++ s t2
  show_term "ilt" [t1,t2] = s t1 ++ " is less than " ++ s t2
  show_term "evalto" [t1,t2] = s t1 ++ " evalto " ++ s t2
  show_term "ite" [t1,t2,t3] = "if "++s t1++" then "++s t2++" else "++s t3
  show_term "+" [t1,t2] = "(" ++ s t1 ++ " + " ++ s t2 ++ ")"
  show_term "-" [t1,t2] = "(" ++ s t1 ++ " - " ++ s t2 ++ ")"
  show_term "*" [t1,t2] = s t1 ++ " * " ++ s t2
  show_term "<" [t1,t2] = s t1 ++ " < " ++ s t2
  show_term "S" [t] = "S(" ++ s t ++ ")"
  show_term "-->" [t1,t2] = s t1 ++ " ---> " ++ s t2
  show_term "-*->" [t1,t2] = s t1 ++ " -*-> " ++ s t2
  show_term "-d->" [t1,t2] = s t1 ++ " -d-> " ++ s t2
  show_term "e=>" [t1,t2,t3] = show_env t1 ++ " |- " ++ s t2 ++ " evalto " ++ s t3
  show_term "let" [t1,t2,t3] = "let "++s t1++" = "++s t2++" in "++s t3
  show_term "vfun" [t1,t2,t3] = "("++show_env t1++")[fun "++s t2++"->"++s t3++"]"
  show_term "vrec" [t1,t2,t3,t4] = "("++show_env t1++")[rec "++s t2++"= fun "++s t3++"->"++s t4++"]"
  show_term "fun" [t1,t2] = "(fun "++s t1++"->"++s t2++")"
  show_term "." [t1,t2] = "("++s t1++" "++s t2++")"
  show_term "letrec" [t1,t2,t3,t4] = "(let rec"++s t1++"=fun "++s t2++"->"++s t3++" in "++s t4++")"

  show_term "d->" [t1,t2,t3] = show_vars t1 ++ " |- " ++ s t2 ++ " evalto " ++ s t3
  show_term "d=>" [t1,t2,t3] = show_db t1 ++ " |- " ++ s t2 ++ " evalto " ++ s t3
  show_term "dlet" [t2,t3] = "let . = "++s t2++" in "++s t3
  show_term "dvfun" [t1,t3] = "("++show_db t1 ++")[fun . -> "++s t3++"]"
  show_term "dvrec" [t1,t4] = "("++show_db t1++")[rec . =fun . -> "++s t4++"]"
  show_term "dfun" [t2] = "(fun . -> "++s t2++")"
  show_term "dletrec" [t3,t4] = "(let rec . = sfun . -> "++s t3++" in "++s t4++")"
  show_term "&" [App t []] = "#" ++ t
  show_term "&" [Var i] = "#$" ++ show i

  show_term "T" [] = "true"
  show_term "F" [] = "false"
  show_term s [] = s
  show_term s t = error $ "Print error: '"++s++show t++"'"

  show_env (App "." []) = ""
  show_env (App "@" [App "=" [App n [], v], l]) = show_env' l++n++"="++s v
  show_env (App "@" [App "=" [Var i, v], l]) = show_env' l++"$"++show i++"="++s v
  show_env (App "@" [Var i, l]) = show_env' l++"$"++show i
  show_env (Var i) = "$" ++ show i
  show_env' s = case show_env s of "" -> ""; s -> s ++ ","

  show_vars (App "." []) = ""
  show_vars (Var i) = "$" ++ show i
  show_vars (App "@" [App t [], l]) = show_vars' l ++ t
  show_vars (App "@" [Var i, l]) = show_vars' l++"$"++show i
  show_vars' s = case show_vars s of "" -> ""; s -> s ++ ","

  show_db (App "." []) = ""
  show_db (Var i) = "$" ++ show i
  show_db (App "@" [t, l]) = show_db' l ++ s t
  show_db' s = case show_db s of "" -> ""; s -> s ++ ","

fromSTerm :: String -> Either String Term
fromSTerm s = make $ tokenizer s ""
  where
    make l = do
      res <- par l
      case res of (s, []) -> Right s 
                  (_, a) -> Left ("Error make :" ++ show a)
    par ("(" : n : c) =
      if head n == '$' then
        return $ (Var (read (tail n)), tail c)
      else do
        (s, c) <- sub c
        return $ (App n s, c)
    par s = Left ("Error par: " ++ show s)
    sub (")":s) = return ([], s)
    sub [] = Left "Error sub"
    sub ("(":t) = do
      (s, c) <- par ("(":t)
      (n, c) <- sub c
      return (s : n, c)
    
    tokens = tokenizer s ""

    tokenizer "" s = if s == "" then [] else [s]
    tokenizer (' ':t) s = (if s == ""
      then tokenizer t ""
      else s : tokenizer t "")
    tokenizer ('(':t) s = (if s == ""
      then "(" : tokenizer t ""
      else s : "(" : tokenizer t "")
    tokenizer (')':t) s = (if s == ""
      then ")" : tokenizer t ""
      else s : ")" : tokenizer t "")
    tokenizer (k:t) s = tokenizer t (s++[k])

parseSterm :: Parsec String () Term
parseSterm = do
  spaces
  string "("
  spaces
  (try $ do {
    string "$";
    n <- many1 digit;
    spaces;
    string ")";
    return $ Var $ read n 
  }) <|> (try $ do {
    string "#";
    n <- many1 digit;
    spaces;
    string ")";
    return $ succnum $ read n
  }) <|> do {
    spaces;
    n <- many1 nonsp;
    spaces;
    m <- many parseSterm;
    spaces;
    string ")";
    return $ App n m;
  }
  where
    nonsp = satisfy (\c -> not (isSpace c || '(' == c || ')' == c))

succnum :: Int -> Term
succnum 0 = App "Z" []
succnum n = App "S" [succnum (n-1)]
numsucc :: Term -> Int
numsucc (App "Z" []) = 0
numsucc (App "S" [t]) = 1 + numsucc t

parseTerm :: Parsec String () Term
parseTerm =
  (try $ do {
    string "S"; spaces;
    string "("; spaces;
    t <- parseTerm;
    string ")"; spaces;
    return $ App "S" [t]
  }) <|> (try $ do {
    string "Z"; spaces;
    return $ App "Z" []
  }) <|> (try $ do {
    string "$";
    n <- many1 digit;
    spaces;
    return $ Var $ read n 
  }) <|> (do {
    t1 <- parseTerm; spaces;
    (try $ do {
      string "is"; spaces;
      t2 <- parseTerm; spaces;
      return $ App "is" [t1, t2]
    }) <|> (try $ do {
      string "plus"; spaces;
      t2 <- parseTerm; spaces;
      return $ App "plus" [t1, t2]
    })
  })
testparse = parseTest parseTerm "Z plus Z"