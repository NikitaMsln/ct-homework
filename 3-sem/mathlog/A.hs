module Main where
 
import Data.List (intercalate)
 
class TokenParse p where
    parse :: String -> [(p, String)]
 
data Token = LeftP | RightP | OrT | AndT | ImplT | NotT | VarT Char | EmptyT deriving (Show, Eq)
 
instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('|':xs) = [(OrT, xs)]
    parse ('&':xs) = [(AndT, xs)]
    parse ('-':'>':xs) = [(ImplT, xs)]
    parse ('!':xs) = [(NotT, xs)]
    parse (x:xs) = if ((fromEnum x) >= (fromEnum 'A') && (fromEnum x) <= (fromEnum 'Z')) then [(VarT x, xs)]
                   else if ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9')) then [(VarT x, xs)]
                   else if (fromEnum x == 39) then [(VarT x, xs)]
                   else [(EmptyT, xs)]
 
safeCall :: (String -> [Token]) -> [(Token, String)] -> [Token]
safeCall f l = if (length l == 0) then []
               else [(fst (head l))] ++ (f (snd (head l)))
 
parseTokens :: String -> [Token]
parseTokens s = safeCall parseTokens (parse s)
 
data Binop = Impl | Or | And
 
instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"
 
data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String
 
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

parseWhileCanImpl :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> [Token] -> [([a], [Token])]
parseWhileCanImpl p ps t = let r = (p t) in
    if (emptyT r) then [([], t)]
    else let rs = (ps (tailT r)) in
        if (emptyT rs) then [([valueT r], tailT r)]
        else let rr = parseWhileCanImpl p ps (tailT rs) in [([valueT r] ++ (valueT rr), tailT rr)]

parseWhileCan :: ([Token] -> [(a, [Token])]) -> ([Token] -> [(b, [Token])]) -> ([a] -> c) -> [Token] -> [(c, [Token])]
parseWhileCan p ps comb t = let r = parseWhileCanImpl p ps t in if (emptyT r) then [] else [(comb (valueT r), tailT r)]
 
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
        (parseWhileCan parseNot (parseToken (isToken AndT) (\x -> "&")) (makeBinop And . reverseList)),
        parseNot
    ]
 
parseOr = parseTry [
        (parseWhileCan parseAnd (parseToken (isToken OrT) (\x -> "|")) (makeBinop Or . reverseList)),
        parseAnd
    ]
 
parseExpr = parseTry [
        (parseCombine (parseCombine parseOr (parseToken (isToken ImplT) (\x -> "->")) fstArg) parseExpr (\x y -> Binary Impl x y)),
        parseOr
    ]
 
parseS :: String -> Expr
parseS x = valueT (parseExpr (filter (\x -> (x /= EmptyT)) (parseTokens x)))
 
main = do
    input <- getLine
    putStrLn (show (parseS input))