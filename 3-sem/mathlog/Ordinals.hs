module Main where

import Data.List (null, elem, intercalate)

class TokenParse p where
    parse :: String -> [(p, String)]

data Token = LeftP | RightP | EqT | PlusT | MultT | PowT | WT | NumT Char | EmptyT deriving (Show, Eq)

isNumChar :: Char -> Bool
isNumChar x = ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9'))

instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('=':xs) = [(EqT, xs)]
    parse ('+':xs) = [(PlusT, xs)]
    parse ('*':xs) = [(MultT, xs)]
    parse ('^':xs) = [(PowT, xs)]
    parse ('w':xs) = [(WT, xs)]
    parse (x:xs) = if (isNumChar x) then [(NumT x, xs)]
                   else [(EmptyT, xs)]

safeCall :: (String -> [Token]) -> [(Token, String)] -> [Token]
safeCall f l = if (length l == 0) then []
               else [(fst (head l))] ++ (f (snd (head l)))

parseTokens :: String -> [Token]
parseTokens s = safeCall parseTokens (parse s)

data Binop = Add | Multy | Pow deriving (Eq)

instance Show Binop where
    show Add = "+"
    show Multy = "*"
    show Pow = "^"

data Expr = Binary Binop [Expr] |
            Var |
            Number Integer deriving (Eq)

instance Show Expr where
    show (Binary Add e) = "(" ++ (intercalate " + " . map show $ e) ++ ")"
    show (Binary op e) = intercalate (show op) . map show $ e
    show (Number n) = show n
    show Var = "w"
    
valueT :: [(a, b)] -> a
valueT s = fst (head s)
    
tailT :: [(a, b)] -> b
tailT s = snd (head s)
    
parseTry :: [([t] -> [(a, [t])])] -> [t] -> [(a, [t])]
parseTry (p:ps) t = if (null r) then (parseTry ps t) else r where r = p t
parseTry [] _ = []
    
parseCombine :: ([t] -> [(a, [t])]) -> ([t] -> [(b, [t])]) -> (a -> b -> c) -> [t] -> [(c, [t])]
parseCombine p1 p2 comb t =
    let r1 = p1 t in
        if (null r1) then []
        else (let r2 = p2 (tailT r1) in
            if (null r2) then []
            else [((comb (valueT r1) (valueT r2)), tailT r2)]
            )
 
parseOptional :: a -> ([t] -> [(a, [t])]) -> [t] -> [(a, [t])]
parseOptional d f t = let res = f t in if (null res) then [(d, t)] else res
 
parseWhileCanImpl :: ([t] -> [(a, [t])]) -> ([t] -> [(b, [t])]) -> [t] -> [([a], [t])]
parseWhileCanImpl p ps t = let r = (p t) in
    if (null r) then [([], t)]
    else let rs = (ps (tailT r)) in
        if (null rs) then [([valueT r], tailT r)]
        else let rr = parseWhileCanImpl p ps (tailT rs) in [((valueT r):(valueT rr), tailT rr)]
   
parseWhileCan :: ([t] -> [(a, [t])]) -> ([t] -> [(b, [t])]) -> ([a] -> c) -> [t] -> [(c, [t])]
parseWhileCan p ps comb t = let r = parseWhileCanImpl p ps t in if (null r || (null . valueT $ r)) then [] else [(comb (valueT r), tailT r)]
    
parseToken :: (t -> Bool) -> (t -> a) -> [t] -> [(a, [t])]
parseToken f c (s:ss) = if (f s) then [(c s, ss)] else []
parseToken _ _ [] = []
   
convert :: (a -> b) -> [(a, [t])] -> [(b, [t])]
convert f = map (\(x, y) -> (f x, y))
 
clear :: (a -> Bool) -> [(a, [t])] -> [(a, [t])]
clear f = filter (\(x, y) -> f x)
    
fstArg :: a -> b -> a
fstArg = const
    
sndArg :: a -> b -> b
sndArg _ b = b

isNumT :: Token -> Bool
isNumT (NumT c) = True
isNumT _ = False

parseNumber :: [Token] -> [(Integer, [Token])]
parseNumber = parseWhileCan (parseToken isNumT (\(NumT c) -> c)) (\t -> [((), t)]) (read :: String -> Integer)

parseEquation :: [Token] -> [((Expr, Expr), [Token])]
parseExpr :: [Token] -> [(Expr, [Token])]
parseAdd :: [Token] -> [(Expr, [Token])]
parseMult :: [Token] -> [(Expr, [Token])]
parseTerm :: [Token] -> [(Expr, [Token])]

parseEquation = parseCombine (parseCombine parseExpr (parseToken ((==) EqT) (\t -> ())) fstArg) parseExpr (\x y -> (x, y))

parseExpr = parseWhileCan parseAdd (parseToken ((==) PlusT) (\t -> ())) (\x -> if ((length x) > 1) then Binary Add x else head x)

parseAdd = parseWhileCan parseMult (parseToken ((==) MultT) (\t -> ())) (\x -> if ((length x) > 1) then Binary Multy x else head x)

parseMult = parseWhileCan parseTerm (parseToken ((==) PowT) (\t -> ())) (\x -> if ((length x) > 1) then Binary Pow x else head x)

parseTerm = parseTry [
        (parseToken ((==) WT) (\x -> Var)),
        (parseCombine (parseToken ((==) LeftP) (\x -> ())) (parseCombine parseExpr (parseToken ((==) RightP) (\x -> ())) fstArg) sndArg),
        convert (\x -> Number x) . parseNumber
    ]

parseLine :: String -> (Expr, Expr)
parseLine = valueT . parseEquation . filter ((/=) EmptyT) . parseTokens

data NormalizedExpr = Seq [(NormalizedExpr, Integer)] Integer deriving (Eq)

instance Ord NormalizedExpr where
    (<=) (Seq ((ax, ay):as) n) (Seq ((bx, by):bs) k) = (ax < bx) || ((ax == bx) && (ay < by)) || (ax == bx && ay == by && (Seq as n) <= (Seq bs k))
    (<=) (Seq [] n) (Seq [] k) = n <= k
    (<=) (Seq a n) (Seq [] k) = False
    (<=) (Seq [] n) (Seq b k) = True

instance Show NormalizedExpr where
    show (Seq [] n) = (show n)
    show (Seq a n) = "(" ++ (intercalate " + " . map (\(x, y) -> "w^" ++ (show x) ++ "*" ++ (show y)) $ a) ++ (if (n /= 0) then " + " ++ (show n) else "") ++ ")"

split :: NormalizedExpr -> [NormalizedExpr]
split (Seq (x:xs) m) = (Seq [x] 0):(split (Seq xs m))
split (Seq [] m) = [(Seq [] m)]

concatTwoNormalizedExpr :: Binop -> NormalizedExpr -> NormalizedExpr -> NormalizedExpr
concatNormalizedExpr :: Binop -> [NormalizedExpr] -> NormalizedExpr

decOrdinal :: (NormalizedExpr, Integer) -> (NormalizedExpr, Bool, Integer)
decOrdinal ((Seq [] k), i) = ((Seq [] (k - 1)), k == 1, i)
decOrdinal ((Seq a n), i) = ((Seq a n), False, i)

decOrdinalsPow :: [(NormalizedExpr, Integer)] -> NormalizedExpr
decOrdinalsPow ((a, i):t) = let (Seq rl rv) = decOrdinalsPow t in let (x, y, z) = decOrdinal (a, i) in if (y) then (Seq rl (rv + z)) else (Seq ((x, z):rl) rv)
decOrdinalsPow [] = (Seq [] 0)

fastPowOrdinal :: NormalizedExpr -> Integer -> NormalizedExpr
fastPowOrdinal a 1 = a
fastPowOrdinal a x | even x = let h = fastPowOrdinal a (div x 2) in concatTwoNormalizedExpr Multy h h
                   | odd x = let h = fastPowOrdinal a (div (x - 1) 2) in concatTwoNormalizedExpr Multy (concatTwoNormalizedExpr Multy h h) a

powOne :: NormalizedExpr -> NormalizedExpr -> NormalizedExpr
powOne b (Seq [] 0) = (Seq [] 1)
powOne b (Seq [] n) = fastPowOrdinal b n
powOne (Seq ((ax, ay):as) n) b = (Seq [(concatTwoNormalizedExpr Multy ax b, 1)] 0)

concatTwoNormalizedExpr Add (Seq ((ax, ay):as) n) (Seq ((bx, by):bs) k) =
    if (ax < bx) then
        (Seq ((bx, by):bs) k)
    else if (ax == bx) then 
        let (Seq rl rn) = concatTwoNormalizedExpr Add (Seq as n) (Seq ((bx, by + ay):bs) k) in
            (Seq rl rn)
    else
        let (Seq rl rn) = concatTwoNormalizedExpr Add (Seq as n) (Seq ((bx, by):bs) k) in
            (Seq ((ax, ay):rl) rn)
concatTwoNormalizedExpr Add (Seq a n) (Seq [] k) = Seq a (n + k)
concatTwoNormalizedExpr Add (Seq [] n) a = a

concatTwoNormalizedExpr Multy (Seq [] 0) b = (Seq [] 0)
concatTwoNormalizedExpr Multy a (Seq [] 0) = (Seq [] 0)
concatTwoNormalizedExpr Multy (Seq [] n) (Seq [] m) = (Seq [] (n * m))
concatTwoNormalizedExpr Multy (Seq ((x, y):t) n) (Seq [] m) = (Seq ((x, y * m):t) n)
concatTwoNormalizedExpr Multy (Seq ((ax, ay):as) n) (Seq [(bx, by)] 0) = (Seq [((concatTwoNormalizedExpr Add ax bx), by)] 0)
concatTwoNormalizedExpr Multy (Seq [] n) (Seq [(bx, by)] m) = (Seq [(bx, by)] (n * m))
concatTwoNormalizedExpr Multy a b = concatNormalizedExpr Add (map (concatTwoNormalizedExpr Multy a) . split $ b)

concatTwoNormalizedExpr Pow (Seq [] 0) b = (Seq [] 1)
concatTwoNormalizedExpr Pow (Seq [] 1) b = b
concatTwoNormalizedExpr Pow a (Seq [] 0) = (Seq [] 0)
concatTwoNormalizedExpr Pow a (Seq [] 1) = (Seq [] 1)
concatTwoNormalizedExpr Pow (Seq [] n) (Seq [] m) = (Seq [] (m ^ n))
concatTwoNormalizedExpr Pow (Seq a m) (Seq [] n) = (Seq [(decOrdinalsPow a, n^m)] 0)
concatTwoNormalizedExpr Pow a b = concatNormalizedExpr Multy . map (powOne b) . split $ a

concatNormalizedExpr _ [x] = x
concatNormalizedExpr op (a:b:t) = concatNormalizedExpr op ((concatTwoNormalizedExpr op a b):t)

normalize :: Expr -> NormalizedExpr
normalize (Number x) = (Seq [] x)
normalize Var = (Seq [((Seq [] 1), 1)] 0)
normalize (Binary Pow l) = concatNormalizedExpr Pow . map normalize . reverse $ l
normalize (Binary op l) = concatNormalizedExpr op . map normalize $ l

printAns :: (NormalizedExpr, NormalizedExpr) -> String
printAns (a, b) | (a == b) = "Равны"
               | (a /= b) = "Не равны"

main = do
    input <- getLine
    putStrLn (printAns . (\(x, y) -> (normalize x, normalize y)) . parseLine $ input)