module Term where

type Token = String

data Term =
    Var Int
  | App Token [Term]
  deriving (Eq, Show, Read)

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


