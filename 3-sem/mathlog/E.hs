module Main where
   
import Data.List (intercalate)
import Data.Set (Set, empty, insert, singleton, member)
   
class TokenParse p where
    parse :: String -> [(p, String)]
   
data Token = LeftP | RightP | OrT | AndT | ImplT | NotT | ForallT | ExistT | CommaT | DotT | VarT Char | EmptyT deriving (Show, Eq)
  
isPredChar :: Char -> Bool
isPredChar x = ((fromEnum x) >= (fromEnum 'A') && (fromEnum x) <= (fromEnum 'Z')) || ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9'))
  
isVarChar :: Char -> Bool
isVarChar x = ((fromEnum x) >= (fromEnum 'a') && (fromEnum x) <= (fromEnum 'z')) || ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9'))
  
instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('|':xs) = [(OrT, xs)]
    parse ('&':xs) = [(AndT, xs)]
    parse ('-':'>':xs) = [(ImplT, xs)]
    parse ('!':xs) = [(NotT, xs)]
    parse ('.':xs) = [(DotT, xs)]
    parse (',':xs) = [(CommaT, xs)]
    parse ('@':xs) = [(ForallT, xs)]
    parse ('?':xs) = [(ExistT, xs)]
    parse (x:xs) = if ((isVarChar x) || (isPredChar x)) then [(VarT x, xs)]
                   else [(EmptyT, xs)]
   
safeCall :: (String -> [Token]) -> [(Token, String)] -> [Token]
safeCall f l = if (length l == 0) then []
               else [(fst (head l))] ++ (f (snd (head l)))
   
parseTokens :: String -> [Token]
parseTokens s = safeCall parseTokens (parse s)
   
data Binop = Impl | Or | And deriving (Eq, Ord)
  
instance Show Binop where
    show Impl = "->"
    show Or = "|"
    show And = "&"
  
data Quantop = Forall | Exist deriving (Eq, Ord)
  
instance Show Quantop where
    show Forall = "@"
    show Exist = "?"
  
data Expr = Quant Quantop Expr Expr
          | Binary Binop Expr Expr
          | Not Expr
          | Func String [Expr]
          | PredFunc String [Expr]
          | Var String
          | Pred String deriving (Eq, Ord)
  
instance Show Expr where
    show (Quant q x y) = "(" ++ (show q) ++ (show x) ++ "." ++ (show y) ++ ")"
    show (Binary op x y) = "(" ++ (show x) ++ (show op) ++ (show y) ++ ")"
    show (Not x) = "(!" ++ (show x) ++ ")"
    show (Func x l) = x ++ "(" ++ (intercalate "," . map show $ l) ++ ")"
    show (PredFunc x l) = x ++ "(" ++ (intercalate "," . map show $ l) ++ ")"
    show (Var x) = x
    show (Pred x) = x
   
emptyT :: [a] -> Bool
emptyT s = (length s == 0)
   
valueT :: [(a, b)] -> a
valueT s = fst (head s)
   
tailT :: [(a, b)] -> b
tailT s = snd (head s)
   
parseTry :: [([Token] -> [(a, [Token])])] -> [Token] -> [(a, [Token])]
parseTry (p:ps) t = if (emptyT r) then (parseTry ps t) else r where r = p t
parseTry [] _ = []
   
parseCombine :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> (a -> b -> c) -> [Token] -> [(c, [Token])]
parseCombine p1 p2 comb t =
    let r1 = p1 t in
        if (emptyT r1) then []
        else (let r2 = p2 (tailT r1) in
            if (emptyT r2) then []
            else [((comb (valueT r1) (valueT r2)), tailT r2)]
            )

parseOptional :: a -> ([Token] -> [(a, [Token])]) -> [Token] -> [(a, [Token])]
parseOptional d f t = let res = f t in if (emptyT res) then [(d, t)] else res

parseWhileCanImpl :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> [Token] -> [([a], [Token])]
parseWhileCanImpl p ps t = let r = (p t) in
    if (emptyT r) then [([], t)]
    else let rs = (ps (tailT r)) in
        if (emptyT rs) then [([valueT r], tailT r)]
        else let rr = parseWhileCanImpl p ps (tailT rs) in [((valueT r):(valueT rr), tailT rr)]
  
parseWhileCan :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> ([a] -> c) -> [Token] -> [(c, [Token])]
parseWhileCan p ps comb t = let r = parseWhileCanImpl p ps t in if (emptyT r || (emptyT . valueT $ r)) then [] else [(comb (valueT r), tailT r)]
   
parseToken :: (Token -> Bool) -> (Token -> a) -> [Token] -> [(a, [Token])]
parseToken f c (s:ss) = if (f s) then [(c s, ss)] else []
parseToken _ _ [] = []
  
makeBinop :: Binop -> [Expr] -> Expr
makeBinop Impl (e:es) = if (emptyT es) then e else (Binary Impl e (makeBinop Impl es))
makeBinop b (e:es) = if (emptyT es) then e else (Binary b (makeBinop b es) e)
   
isToken :: Token -> Token -> Bool
isToken a b = (a == b)
   
fstArg :: a -> b -> a
fstArg = const
   
sndArg :: a -> b -> b
sndArg _ b = b
   
parseSeq :: (Token -> Bool) -> (Token -> a) -> [Token] -> [([a], [Token])]
parseSeq f g (x:xs) = if (f x) then let res = parseSeq f g xs in [(((g x):(valueT res)), tailT res)] else [([], (x:xs))]
parseSeq f g [] = [([], [])]
  
convert :: (a -> b) -> [(a, [Token])] -> [(b, [Token])]
convert f = map (\(x, y) -> (f x, y))

clear :: (a -> Bool) -> [(a, [Token])] -> [(a, [Token])]
clear f = filter (\(x, y) -> f x)
  
filterVar :: (Char -> Bool) -> Token -> Bool
filterVar f (VarT c) = f c
filterVar _ _ = False
  
parseExpr :: [Token] -> [(Expr, [Token])]
parseOr :: [Token] -> [(Expr, [Token])]
parseAnd :: [Token] -> [(Expr, [Token])]
parseUnar :: [Token] -> [(Expr, [Token])]
parsePred :: [Token] -> [(Expr, [Token])]
parseTerm :: [Token] -> [(Expr, [Token])]
parseVar :: [Token] -> [(String, [Token])]
parsePredVar :: [Token] -> [(String, [Token])]
  
parseVar = clear (\x -> x /= "") . parseSeq (filterVar isVarChar) (\(VarT c) -> c)
  
parsePredVar = clear (\x -> x /= "") . parseSeq (filterVar isPredChar) (\(VarT c) -> c)

parseBrackets :: [Token] -> [(Maybe [Expr], [Token])]
parseBrackets = parseOptional Nothing (parseCombine (parseCombine (parseToken (isToken LeftP) (\x -> ())) (parseOptional (Just []) (convert (\x -> Just x) . parseWhileCan parseTerm (parseToken (isToken CommaT) (\x -> ",")) (\x -> x))) sndArg) (parseToken (isToken RightP) (\x -> ())) fstArg)

buildFunc :: String -> Maybe [Expr] -> Expr
buildFunc s Nothing = Var s
buildFunc s (Just l) = Func s l

buildPred :: String -> Maybe [Expr] -> Expr
buildPred s Nothing = Pred s
buildPred s (Just []) = Pred s
buildPred s (Just l) = PredFunc s l

parseTerm (LeftP:ts) = parseCombine parseTerm (parseToken (isToken RightP) (\x -> ())) fstArg ts
parseTerm t = parseCombine parseVar parseBrackets buildFunc t
  
parsePred = parseCombine parsePredVar parseBrackets buildPred

parseUnar (ForallT:ts) = parseCombine parseVar (parseCombine (parseToken (isToken DotT) (\x -> ())) parseExpr sndArg) (\x y -> (Quant Forall (Var x) y)) ts
parseUnar (ExistT:ts) = parseCombine parseVar (parseCombine (parseToken (isToken DotT) (\x -> ())) parseExpr sndArg) (\x y -> (Quant Exist (Var x) y)) ts
parseUnar (NotT:ts) = convert (\x -> Not x) . parseUnar $ ts
parseUnar (LeftP:ts) = parseCombine parseExpr (parseToken (isToken RightP) (\x -> ())) fstArg ts
parseUnar t = parsePred t
   
parseAnd = (parseCombine parseUnar (parseOptional [] (parseCombine (parseToken (isToken AndT) (\x -> ())) (parseWhileCan parseUnar (parseToken (isToken AndT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinop And . reverse $ (x:y)))
   
parseOr = (parseCombine parseAnd (parseOptional [] (parseCombine (parseToken (isToken OrT) (\x -> ())) (parseWhileCan parseAnd (parseToken (isToken OrT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinop Or . reverse $ (x:y)))
   
parseExpr = (parseCombine parseOr (parseOptional [] (parseCombine (parseToken (isToken ImplT) (\x -> ())) (parseWhileCan parseOr (parseToken (isToken ImplT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinop Impl (x:y)))
  
parseS :: String -> Expr
parseS x = valueT (parseExpr (filter (\x -> (x /= EmptyT)) (parseTokens x)))
  
-- [BEGIN] Axiom check
   
bimap :: (a -> b -> c) -> [a] -> [b] -> [c]
bimap f (x:xs) (y:ys) = (f x y):(bimap f xs ys)
bimap _ [] [] = []
   
termap :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
termap f x y z = bimap (\x y -> x y) (bimap f x y) z
   
fold :: (a -> a -> a) -> a -> [a] -> a
fold f a (x:xs) = f x (fold f a xs)
fold f a _ = a
   
checkSubstEquality :: Maybe Expr -> Maybe Expr -> (Bool, Maybe Expr)
checkSubstEquality a Nothing = (True, a)
checkSubstEquality Nothing b = (True, b)
checkSubstEquality (Just a) (Just b) = (a == b, Just a)
   
checkSubstEqualityMulty :: [Maybe Expr] -> (Bool, Maybe Expr)
checkSubstEqualityMulty [] = (True, Nothing)
checkSubstEqualityMulty (x:xs) = let (v, r) = checkSubstEqualityMulty xs in let (vf, rf) = checkSubstEquality x r in (v && vf, rf)
   
checkSubstitution :: Expr -> Expr -> Expr -> (Bool, Maybe Expr)
checkSubstitution e (Binary op a b) (Binary ops as bs) =
    (v, r) where
        (xv, xr) = checkSubstitution e a as
        (yv, yr) = checkSubstitution e b bs
        (rv, r) = checkSubstEquality xr yr
        v = (op == ops) && xv && yv && rv
   
checkSubstitution e (Quant t a b) (Quant ts as bs) =
    if (a == e) then
        ((ts == t) && (a == as) && (b == bs), Nothing)
    else
        (v, r) where
            (yv, r) = checkSubstitution e b bs
            v = (t == ts) && (a == as) && yv
   
checkSubstitution e (Not a) (Not as) = checkSubstitution e a as
   
checkSubstitution (Var s) (Var ss) es = if (s == ss) then (True, Just es) else ((Var ss) == es, Nothing)
   
checkSubstitution e (Func a l) (Func as ls) =
    if ((length l) == (length ls)) then
        let lr = bimap (checkSubstitution e) l ls in let (rv, re) = checkSubstEqualityMulty . map snd $ lr in
            (a == as && (fold (&&) True . map fst $ lr) && rv, re)
    else
        (False, Nothing)
   
checkSubstitution e (Pred a) (Pred as) = (a == as, Nothing)
checkSubstitution e (PredFunc a l) (PredFunc as ls) =
    if ((length l) == (length ls)) then
        let lr = bimap (checkSubstitution e) l ls in let (rv, re) = checkSubstEqualityMulty . map snd $ lr in
            (a == as && (fold (&&) True . map fst $ lr) && rv, re)
    else
        (False, Nothing)
   
checkSubstitution _ _ _ = (False, Nothing)
   
choseJust :: Maybe a -> a -> a
choseJust Nothing x = x
choseJust (Just x) _ = x
   
data IndependenceTree = Node [IndependenceTree] |
                        VarLeaf Bool |
                        EmptyLeaf deriving (Eq)
   
buildIndependenceTreeImpl :: (Set String) -> Expr -> IndependenceTree
buildIndependenceTreeImpl s (Binary op a b) = Node [(buildIndependenceTreeImpl s a), (buildIndependenceTreeImpl s b)]
buildIndependenceTreeImpl s (Quant t (Var a) b) = Node [(buildIndependenceTreeImpl (insert a s) b)]
buildIndependenceTreeImpl s (Not a) = Node [(buildIndependenceTreeImpl s a)]
buildIndependenceTreeImpl s (Var a) = VarLeaf (member a s)
buildIndependenceTreeImpl s (Func a l) = Node (map (buildIndependenceTreeImpl s) l)
buildIndependenceTreeImpl s (Pred a) = EmptyLeaf
buildIndependenceTreeImpl s (PredFunc a l) = Node (map (buildIndependenceTreeImpl s) l)
   
buildIndependenceTree :: Expr -> IndependenceTree
buildIndependenceTree = buildIndependenceTreeImpl empty
   
checkSubstIndependence :: Expr -> Expr -> IndependenceTree -> (Expr, IndependenceTree) -> Bool
checkSubstIndependence e (Binary op a b) t ((Binary ops as bs), (Node (axs:bxs:xss))) = (checkSubstIndependence e a t (as, axs)) && (checkSubstIndependence e b t (bs, bxs))
checkSubstIndependence e (Quant _ (Var a) b) t ((Quant _ as bs), (Node (bxs:xss))) = (e == (Var a) && b == bs) || (checkSubstIndependence e b t (bs, bxs))
checkSubstIndependence e (Not a) t ((Not as), (Node (axs:xss))) = checkSubstIndependence e a t (as, axs)
checkSubstIndependence e (Var a) t (as, ts) = (e /= (Var a)) || (t == ts)
checkSubstIndependence e (Func a l) t ((Func as ls), (Node xs)) = fold (&&) True . termap (\x y z -> checkSubstIndependence e x t (y, z)) l ls $ xs
checkSubstIndependence _ (Pred a) _ _ = True
checkSubstIndependence e (PredFunc a l) t ((PredFunc as ls), (Node xs)) = fold (&&) True . termap (\x y z -> checkSubstIndependence e x t (y, z)) l ls $ xs
   
   
isAxiom11 :: Expr -> Maybe (Bool, Expr, Expr, Expr)
isAxiom11 (Binary Impl (Quant Forall x phi) oth) =
    let (rv, re) = checkSubstitution x phi oth in
        if (rv) then
            Just (checkSubstIndependence x phi (buildIndependenceTree (choseJust re x)) (oth, (buildIndependenceTree oth)), x, phi, (choseJust re x))
        else Nothing
isAxiom11 _ = Nothing
   
isAxiom12 :: Expr -> Maybe (Bool, Expr, Expr, Expr)
isAxiom12 (Binary Impl oth (Quant Exist x phi)) =
    let (rv, re) = checkSubstitution x phi oth in
        if (rv) then
            Just (checkSubstIndependence x phi (buildIndependenceTree (choseJust re x)) (oth, (buildIndependenceTree oth)), x, phi, (choseJust re x))
        else Nothing
isAxiom12 _ = Nothing
   
showMessage :: Maybe (Bool, Expr, Expr, Expr) -> Maybe (Bool, Expr, Expr, Expr) -> String
showMessage Nothing Nothing = "Not an axiom scheme 11 or 12"
showMessage (Just (True, x, phi, theta)) _ = "Axiom scheme 11, phi = " ++ (show phi) ++ ", x = " ++ (show x) ++ ", theta = " ++ (show theta)
showMessage _ (Just (True, x, phi, theta)) = "Axiom scheme 12, phi = " ++ (show phi) ++ ", x = " ++ (show x) ++ ", theta = " ++ (show theta)
showMessage (Just (False, x, phi, theta)) _ = "Similar to axiom scheme 11, phi = " ++ (show phi) ++ ", x = " ++ (show x) ++ ", theta = " ++ (show theta)
showMessage _ (Just (False, x, phi, theta)) = "Similar to axiom scheme 12, phi = " ++ (show phi) ++ ", x = " ++ (show x) ++ ", theta = " ++ (show theta)
   
checkAxiom :: Expr -> String
checkAxiom e = showMessage (isAxiom11 e) (isAxiom12 e)
   
main = do
    input <- getLine
    putStrLn (checkAxiom . parseS $ input)