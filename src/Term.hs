module Term where

import Text.Parsec
import Control.Applicative ((<$>), (<*>))

type Token = String

data Term =
    Var Int
  | App Token [Term]
  deriving (Eq, Show, Read)

showTerm (Var i) = "$" ++ show i
showTerm (App n ts) = show_term n (map showTerm ts)
  where
  show_term "is" [t1,t2] = t1 ++ " is " ++ t2
  show_term "plus" [t1,t2] = t1 ++ " plus " ++ t2
  show_term "ilt" [t1,t2] = t1 ++ " is less than " ++ t2
  show_term "S" [t] = "S(" ++ t ++ ")"
  show_term "Z" [] = "Z"
  show_term s t = error $ "On '"++s++show t++"'"

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
  }) <|> do {
    spaces;
    n <- many1 alphaNum;
    spaces;
    m <- many parseSterm;
    spaces;
    string ")";
    return $ App n m;
  }

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