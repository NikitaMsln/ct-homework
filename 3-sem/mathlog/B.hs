module Main where
   
import Data.List (intercalate, sort)
import System.IO (isEOF)
  
--- [BEGIN] Expression parser
   
class TokenParse p where
    parse :: String -> [(p, String)]
   
data Token = LeftP | RightP | OrT | AndT | ImplT | NotT | VarT Char | CommaT | HypT | EmptyT deriving (Show, Eq)
   
instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('|':'-':xs) = [(HypT, xs)]
    parse ('|':xs) = [(OrT, xs)]
    parse ('&':xs) = [(AndT, xs)]
    parse ('-':'>':xs) = [(ImplT, xs)]
    parse ('!':xs) = [(NotT, xs)]
    parse (',':xs) = [(CommaT, xs)]
    parse (x:xs) = if ((fromEnum x) >= (fromEnum 'A') && (fromEnum x) <= (fromEnum 'Z')) then [(VarT x, xs)]
                   else if ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9')) then [(VarT x, xs)]
                   else if (fromEnum x == 39) then [(VarT x, xs)]
                   else [(EmptyT, xs)]
   
safeCall :: (String -> [Token]) -> [(Token, String)] -> [Token]
safeCall f l = if (length l == 0) then []
               else [(fst (head l))] ++ (f (snd (head l)))
   
parseTokens :: String -> [Token]
parseTokens s = safeCall parseTokens (parse s)
   
data Binop = Impl | Or | And deriving (Eq, Ord)
   
instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"
   
data Expr = Binary Binop Expr Expr | Not Expr | Var String deriving (Eq, Ord)
   
instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "," [show op, show a, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name
   
emptyT :: [a] -> Bool
emptyT s = (length s == 0)
   
valueT :: [(a, b)] -> a
valueT s = fst (head s)
   
tailT :: [(a, b)] -> b
tailT s = snd (head s)
   
parseTry :: [([Token] -> [(a, [Token])])] -> [Token] -> [(a, [Token])]
parseTry _ [] = []
parseTry (p:ps) t = if (emptyT r) then (parseTry ps t) else r where r = p t
parseTry [] _ = []
   
parseCombine :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> (a -> b -> c) -> [Token] -> [(c, [Token])]
parseCombine _ _ _ [] = []
parseCombine p1 p2 comb t =
    let r1 = p1 t in
        if (emptyT r1) then []
        else (let r2 = p2 (tailT r1) in
            if (emptyT r2) then []
            else [((comb (valueT r1) (valueT r2)), tailT r2)]
            )
  
parseWhileCanImpl :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> [Token] -> [([a], [Token])]
parseWhileCanImpl _ _ [] = [([], [])] 
parseWhileCanImpl p ps t = let r = (p t) in
    if (emptyT r) then [([], t)]
    else let rs = (ps (tailT r)) in
        if (emptyT rs) then [([valueT r], tailT r)]
        else let rr = parseWhileCanImpl p ps (tailT rs) in [([valueT r] ++ (valueT rr), tailT rr)]
  
parseWhileCan :: Bool -> ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> ([a] -> c) -> [Token] -> [(c, [Token])]
parseWhileCan f p ps comb t = let r = parseWhileCanImpl p ps t in if (emptyT r || (f && (emptyT (valueT r)))) then [] else [(comb (valueT r), tailT r)]
  
parseWhileCanAny :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> ([a] -> c) -> [Token] -> [(c, [Token])]
parseWhileCanAny = parseWhileCan False
  
parseWhileCanAtLeastOne :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> ([a] -> c) -> [Token] -> [(c, [Token])]
parseWhileCanAtLeastOne = parseWhileCan True
   
parseToken :: (Token -> Bool) -> (Token -> a) -> [Token] -> [(a, [Token])]
parseToken f c (s:ss) = if (f s) then [(c s, ss)] else []
parseToken _ _ [] = []
  
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]
  
makeBinop :: Binop -> [Expr] -> Expr
makeBinop b (e:es) = if (emptyT es) then e else (Binary b (makeBinop b es) e)
   
isToken :: Token -> Token -> Bool
isToken a b = (a == b)
   
fstArg :: a -> b -> a
fstArg = const
   
sndArg :: a -> b -> b
sndArg _ b = b
   
isVarT :: Token -> Bool
isVarT (VarT c) = True
isVarT _ = False
   
getCharFromVarT :: Token -> Char
getCharFromVarT (VarT c) = c
   
pushToVar :: Char -> Expr -> Expr
pushToVar c (Var s) = Var (c:s)
   
parseVarImpl :: [Token] -> [(Expr, [Token])]
parseVarImpl (VarT c:ts) = let x = (parseVarImpl ts) in [(pushToVar c (valueT x), tailT x)]
parseVarImpl s = [(Var "", s)]
   
parseVar :: [Token] -> [(Expr, [Token])]
parseVar (VarT c:ts) = let x = (parseVarImpl ts) in [(pushToVar c (valueT x), tailT x)]
parseVar _ = []
   
parseExpr :: [Token] -> [(Expr, [Token])]
parseOr :: [Token] -> [(Expr, [Token])]
parseAnd :: [Token] -> [(Expr, [Token])]
parseNot :: [Token] -> [(Expr, [Token])]
   
parseNot = parseTry [
        (parseCombine (parseToken (isToken NotT) (\x -> "!")) parseNot (\_ x -> Not x)),
        parseVar,
        (parseCombine (parseCombine (parseToken (isToken LeftP) (\x -> "(")) parseExpr sndArg) (parseToken (isToken RightP) (\x -> "(")) fstArg)
    ]
   
parseAnd = parseTry [
        (parseWhileCanAtLeastOne parseNot (parseToken (isToken AndT) (\x -> "&")) (makeBinop And . reverseList)),
        parseNot
    ]
   
parseOr = parseTry [
        (parseWhileCanAtLeastOne parseAnd (parseToken (isToken OrT) (\x -> "|")) (makeBinop Or . reverseList)),
        parseAnd
    ]
   
parseExpr = parseTry [
        (parseCombine (parseCombine parseOr (parseToken (isToken ImplT) (\x -> "->")) fstArg) parseExpr (\x y -> Binary Impl x y)),
        parseOr
    ]
   
parseS :: String -> Expr
parseS x = valueT (parseExpr (filter (\x -> (x /= EmptyT)) (parseTokens x)))
  
--- [END] Expression parser
  
data ProofWay = Axiom Int | Hupothes Int | MP Int Int | Deduction Int | Incorrect deriving (Eq)
  
instance Show ProofWay where
    show (Axiom i) = "[Ax. sch. " ++ (show i) ++ "]"
    show (Hupothes i) = "[Hyp. " ++ (show i) ++ "]"
    show (MP i j) = "[M.P. "++ (show i) ++ ", " ++ (show j) ++ "]"
    show (Deduction i) = "[Ded. " ++ (show i) ++ "]"
    show (Incorrect) = "[Incorrect]"
  
data MetaExpr = MetaImpl MetaExpr MetaExpr | MetaOr MetaExpr MetaExpr | MetaAnd MetaExpr MetaExpr | MetaNot MetaExpr | MetaVar Int deriving (Eq, Show)
  
pushToSnd :: b -> (a, [b]) -> (a, [b])
pushToSnd v (x, y) = (x, v:y)
  
containsExpr :: (Expr, Int) -> [(Expr, Int)] -> (Bool, [(Expr, Int)])
containsExpr (e, i) ((em, im):ms) = if (i == im) then if (e == em) then (True, (em, im):ms) else (False, (em, im):ms)
                                   else pushToSnd (em, im) (containsExpr (e, i) ms)
containsExpr (e, i) [] = (True, [(e, i)])
  
isMetaExprOfExpr :: MetaExpr -> Expr -> [(Expr, Int)] -> (Bool, [(Expr, Int)])
isMetaExprOfExpr (MetaVar i) e v = containsExpr (e, i) v
isMetaExprOfExpr (MetaNot a) (Not e) v = isMetaExprOfExpr a e v
isMetaExprOfExpr (MetaAnd a b) (Binary And e1 e2) v = let r = isMetaExprOfExpr a e1 v in if (fst r) then isMetaExprOfExpr b e2 (snd r) else r
isMetaExprOfExpr (MetaOr a b) (Binary Or e1 e2) v = let r = isMetaExprOfExpr a e1 v in if (fst r) then isMetaExprOfExpr b e2 (snd r) else r
isMetaExprOfExpr (MetaImpl a b) (Binary Impl e1 e2) v = let r = isMetaExprOfExpr a e1 v in if (fst r) then isMetaExprOfExpr b e2 (snd r) else r
isMetaExprOfExpr _ _ _ = (False, [])
  
axioms = [
        ((MetaVar 1) `MetaImpl` ((MetaVar 2) `MetaImpl` (MetaVar 1))),
        (((MetaVar 1) `MetaImpl` (MetaVar 2)) `MetaImpl` (((MetaVar 1) `MetaImpl` ((MetaVar 2) `MetaImpl` ( MetaVar 3))) `MetaImpl` ((MetaVar 1) `MetaImpl` (MetaVar 3)))),
        ((MetaVar 1) `MetaImpl` ((MetaVar 2) `MetaImpl` ((MetaVar 1) `MetaAnd` (MetaVar 2)))),
        (((MetaVar 1) `MetaAnd` (MetaVar 2)) `MetaImpl` (MetaVar 1)),
        (((MetaVar 1) `MetaAnd` (MetaVar 2)) `MetaImpl` (MetaVar 2)),
        ((MetaVar 1) `MetaImpl` ((MetaVar 1) `MetaOr` (MetaVar 2))),
        ((MetaVar 2) `MetaImpl` ((MetaVar 1) `MetaOr` (MetaVar 2))),
        (((MetaVar 1) `MetaImpl` (MetaVar 3)) `MetaImpl` (((MetaVar 2) `MetaImpl` (MetaVar 3)) `MetaImpl` (((MetaVar 1) `MetaOr` (MetaVar 2)) `MetaImpl` (MetaVar 3)))),
        (((MetaVar 1) `MetaImpl` (MetaVar 3)) `MetaImpl` (((MetaVar 1) `MetaImpl` (MetaNot (MetaVar 3))) `MetaImpl` (MetaNot (MetaVar 1)))),
        ((MetaNot (MetaNot (MetaVar 1))) `MetaImpl` (MetaVar 1))
    ] :: [MetaExpr]
  
checkAxiom :: Expr -> [MetaExpr] -> Int -> ProofWay
checkAxiom e (a:as) n = if (fst (isMetaExprOfExpr a e [])) then (Axiom n) else (checkAxiom e as (n + 1))
checkAxiom e [] n = Incorrect
  
isSamePermutation :: [Expr] -> [Expr] -> Bool
isSamePermutation x y = (sort x) == (sort y)
  
findAxiom :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findAxiom e _ _ = checkAxiom e axioms 1
  
findHypothes :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findHypothes e _ [] = Incorrect
findHypothes e _ ((i, h):hs) = if (e == h) then (Hupothes i) else (findHypothes e [] hs)

findExprOfMP :: Expr -> [Expr] -> [(Int, Expr, [(Int, Expr)])] -> Int
findExprOfMP _ _ [] = -1
findExprOfMP e h ((i, x, xh):xs) = if (e == x && (isSamePermutation h (map snd xh))) then i else findExprOfMP e h xs

findMPImpl :: Expr -> [Expr] -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr, [(Int, Expr)])] -> ProofWay
findMPImpl _ _ [] _ = Incorrect
findMPImpl e h ((i1, e1, h1):l1) l2 = let r = (isMetaExprOfExpr ((MetaVar 1) `MetaImpl` (MetaVar 2)) e1 [(e, 2)]) in
    if (fst r && (isSamePermutation h (map snd h1))) then
        (let r1 = findExprOfMP (fst . head . tail . snd $ r) h l2 in
            if (r1 == -1) then findMPImpl e h l1 l2
            else (MP r1 i1)
        )
    else findMPImpl e h l1 l2
  
findMP :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findMP e l h = (findMPImpl e (map snd h) l l)
  
getImplWay :: Expr -> ([Expr], Expr)
getImplWay (Binary Impl e1 e2) = let (l, r) = (getImplWay e2) in (e1:l, r)
getImplWay e = ([], e)
  
isDeducted :: Expr -> [(Int, Expr)] -> Int -> Expr -> [(Int, Expr)] -> ProofWay
isDeducted e eh i c ch = let (es, ei) = (getImplWay e) in let (cs, ci) = (getImplWay c) in if (ei == ci && (isSamePermutation (es ++ (map snd eh)) (cs ++ (map snd ch)))) then (Deduction i) else Incorrect
  
findDeduct :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findDeduct _ [] _ = Incorrect
findDeduct e ((i, c, ch):cs) h = let r = (isDeducted e h i c ch) in if (r == Incorrect) then (findDeduct e cs h) else r
  
findProof :: [(Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay)] -> Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findProof [] _ _ _ = Incorrect
findProof (c:cs) e l h = let x = (c e l h) in if (x == Incorrect) then (findProof cs e l h) else x
  
--- [BEGIN] Proof parser
  
makeNumberedListImpl :: Int -> [a] -> [(Int, a)]
makeNumberedListImpl _ [] = []
makeNumberedListImpl i (x:xs) = (i, x):(makeNumberedListImpl (i + 1) xs)
  
makeNumberedList :: [a] -> [(Int, a)]
makeNumberedList = makeNumberedListImpl 1
  
parseHypoth :: [Token] -> [([(Int, Expr)], [Token])]
parseHypoth = parseWhileCanAny parseExpr (parseToken (isToken CommaT) (\x -> ',')) makeNumberedList
  
formatLine :: Int -> [(Int, Expr)] -> Expr -> (Int, Expr, [(Int, Expr)])
formatLine i l e = (i, e, l)
  
parseLine :: Int -> [Token] -> [((Int, Expr, [(Int, Expr)]), [Token])]
parseLine i = (parseCombine (parseCombine parseHypoth (parseToken (isToken HypT) (\x -> "|-")) fstArg) parseExpr (formatLine i))
  
parseLineStr :: (Int, String) -> (Int, Expr, [(Int, Expr)])
parseLineStr (i, s) = valueT (parseLine i (filter (\x -> (x /= EmptyT)) (parseTokens s)))
  
parseLines :: [String] -> Int -> [(Int, Expr, [(Int, Expr)])] -> String
parseLines [] _ _ = ""
parseLines (x:xs) i l = let (j, e, h) = (parseLineStr (i, x)) in ("[" ++ (show j) ++ "] " ++ x ++ " " ++ (show (findProof [findAxiom, findHypothes, findMP, findDeduct] e l h)) ++ ['\n'] ++ (parseLines xs (i + 1) ((j, e, h):l)))
  
--- [END] Proof parser
  
inputLines :: IO [String]
inputLines = fmap reverse (go []) where 
        go xs = do  
            done <- isEOF
            if (done)
                then (return xs)
                else do 
                    inp <- getLine 
                    go (inp:xs)
  
main = do
    input <- inputLines
    putStrLn (parseLines input 1 [])