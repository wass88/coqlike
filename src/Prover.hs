module Prover where
  import Proof
  import Term as T
  import Nat as Nat
  import Control.Monad.State as S
  import Text.Parsec
  import Control.Applicative ((<$>), (<*>))
  import Data.Maybe
  import Data.List
  import Data.Char
  import Data.List.Split

  liftS :: Monad m => S.State s a -> StateT s m a
  liftS = StateT . (return .) . runState

  prover :: IO ()
  prover = do
    runStateT proverST ProofState{axiom=[], proof=Nothing}
    return ()
  proverST :: StateT ProofState IO ()
  proverST = do
    ls <- lift getContents
    forM_ (splitOn ";" ls) (\l -> do
      lift $ do{putStr "<<< "; print l; putStr "\n"}
      case readCmd l of
        Left err -> lift $ print err
        Right c -> do 
          err <- liftS $ step c 
          case err of
            Just err -> lift $ putStrLn err
            Nothing -> return ()
          return ()
      s <- liftS $ get
      lift $ putStrLn $ showState s
      )

  data Cmd =
    Axiom Rule |
    Theorem T.Term |
    Apply String |
    ApplyWith String [(Int, T.Term)] |
    Nat |
    Qed |
    ShowRules
    deriving (Eq, Show)
  
  data ProofState = ProofState {
    axiom :: [Rule],
    proof :: Maybe Proof
  }

  showState :: ProofState -> String
  showState ProofState{axiom=a, proof=p} =
    intercalate "/" (map (nameOfRule) a) ++ "\n" ++
    show p

  nonsp = satisfy (not . isSpace)
  readCmd :: String -> Either ParseError Cmd
  readCmd = parse readcmd ""
    where
      readcmd =
        (try $ do {
          spaces; string "ax"; spaces;
          n <- many1 nonsp; spaces;
          ts <- many1 $ (do {
            k <- parseSterm;spaces;return k});
          return $ Axiom $ Rule n (tail ts) (head ts)
        }) <|> (try $ do {
          spaces; string "th"; spaces;
          t <- parseSterm;
          return $ Theorem t
        }) <|> (try $ do {
          spaces; string "ap"; spaces;
          n <- many1 nonsp; spaces;
          return $ Apply n
        }) <|> (try $ do {
          spaces; string "aw"; spaces;
          n <- many1 nonsp; spaces;
          b <- many $ (do {
            n <- many1 digit;
            string ":";
            t <- parseSterm;
            spaces;
            return (read n, t)
          });
          return $ ApplyWith n b
        }) <|> (try $ do {
          spaces; string "qed"; spaces;
          return $ Qed
        }) <|> (try $ do {
          spaces; string "rs"; spaces;
          return $ ShowRules
        }) <|> (try $ do {
          spaces; string "nat"; spaces;
          return $ Nat
        })


  step :: Cmd -> S.State ProofState (Maybe String)
  step (Axiom rule) = do
    s <- get
    put $ s {axiom = rule : axiom s}
    return Nothing
  step (Theorem t) = do
    s <- get
    case proof s of
      Nothing -> do
        put $ s {proof = Just $ startProof t}
        return Nothing
      _ -> return $ Just "has proof"
  step (Apply name) = do
    rule <- getRuleM name
    case rule of
      Nothing -> return $ Just "missing Rule"
      Just rule -> touchProof (apply rule)
  step (ApplyWith name bs) = do
    rule <- getRuleM name
    case rule of
      Nothing -> return $ Just "missing Rule"
      Just rule -> touchProof (applyWith rule bs)
  step (Nat) = do
    touchProof Nat.natProve
  step Qed = do
    s <- get
    case proof s of
      Nothing -> return $ Just "no proof"
      Just p -> if endProof p
        then do put $ s {proof = Nothing}; return $ Nothing
        else return $ Just "has proof"
  step ShowRules = do
    s <- get
    return $ Just (
      intercalate "\n\n" (map (\(Rule n ts t) ->
        n ++ ":\n  " ++ (intercalate "\n  ") (map showTerm ts) ++
        "\n  ->" ++ showTerm t
        ) $ axiom s)
      )
      
  touchProof :: (Proof -> Either String Proof) -> S.State ProofState (Maybe String)
  touchProof fn = do
    s <- get
    case proof s of
      Nothing -> return $ Just "no proof"
      Just f -> do
        case fn f of
          Right f -> do put $ s {proof = Just f}; return $ Nothing
          Left s -> return $ Just s

  getRuleM :: String -> S.State ProofState (Maybe Rule)
  getRuleM name = do
    s <- get
    case getRule (axiom s) name of
      Just rule -> return $ Just rule
      Nothing -> return $ Nothing