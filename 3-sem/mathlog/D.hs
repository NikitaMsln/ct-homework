module Main where
  
import Data.List (intercalate, elem)
import qualified Data.Set as S (Set, insert, union, singleton, empty, toList)
  
class TokenParse p where
    parse :: String -> [(p, String)]
  
data Token = LeftP | RightP | OrT | AndT | ImplT | FalseT | VarT Char | EmptyT deriving (Show, Eq)
  
instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('|':xs) = [(OrT, xs)]
    parse ('&':xs) = [(AndT, xs)]
    parse ('-':'>':xs) = [(ImplT, xs)]
    parse ('_':'|':'_':xs) = [(FalseT, xs)]
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
  
data Expr = Binary Binop Expr Expr
          | Var String
          | FalseE deriving (Eq, Ord)
  
instance Show Expr where
  show (Binary op a b) = "(" ++ (show a) ++ (show op) ++ (show b) ++ ")"
  show (Var name)      = name
  show (FalseE)        = "_|_"
 
infixl 6 |-&
infixl 5 |-|
infixr 4 |-->
         
(|-&) :: Expr -> Expr -> Expr
a |-& b = (Binary And a b)
(|-|) :: Expr -> Expr -> Expr
a |-| b = (Binary Or a b)
(|-->) :: Expr -> Expr -> Expr
a |--> b = (Binary Impl a b)
 
_l_ = FalseE :: Expr
  
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
 
parseSingleExpr :: [Token] -> [(Expr, [Token])]
parseSingleExpr (FalseT:ts) = [(FalseE, ts)]
parseSingleExpr l = parseVar l
  
parseExpr :: [Token] -> [(Expr, [Token])]
parseOr :: [Token] -> [(Expr, [Token])]
parseAnd :: [Token] -> [(Expr, [Token])]
parseVal :: [Token] -> [(Expr, [Token])]
  
parseVal = parseTry [
        parseSingleExpr,
        (parseCombine (parseCombine (parseToken (isToken LeftP) (\x -> "(")) parseExpr sndArg) (parseToken (isToken RightP) (\x -> "(")) fstArg)
    ]
  
parseAnd = parseTry [
        (parseWhileCan parseVal (parseToken (isToken AndT) (\x -> "&")) (makeBinop And . reverseList)),
        parseVal
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
 
-- [BEGIN] ProofTree
 
data SinConn = IIMplP | ElAndP | ErAndP | IlOrP | IrOrP | ENotP
 
instance Show SinConn where
    show IIMplP = "[I->]"
    show ElAndP = "[El&]"
    show ErAndP = "[Er&]"
    show IlOrP = "[Il|]"
    show IrOrP = "[Ir|]"
    show ENotP = "[E!!]"
 
data BinConn = EImplP | IAndP
 
instance Show BinConn where
    show EImplP = "[E->]"
    show IAndP = "[I&]"
 
data TriConn = EOrP
 
instance Show TriConn where
    show EOrP = "[E|]"
 
data ProofTree = AxP Expr [Expr] |
                 SingleP SinConn Expr [Expr] ProofTree |
                 BinaryP BinConn Expr [Expr] ProofTree ProofTree |
                 TripleP TriConn Expr [Expr] ProofTree ProofTree ProofTree
 
showContext :: [Expr] -> String
showContext = intercalate "," . map show
 
toString :: ProofTree -> Int -> String
toString (AxP e c) i = "[" ++ (show i) ++ "] " ++ (showContext c) ++ "|-" ++ (show e) ++ " [Ax]\n"
toString (SingleP t e c g1) i = (toString g1 (i + 1)) ++ "[" ++ (show i) ++ "] " ++ (showContext c) ++ "|-" ++ (show e) ++ " " ++ (show t) ++ "\n"
toString (BinaryP t e c g1 g2) i = (toString g1 (i + 1)) ++ (toString g2 (i + 1)) ++ "[" ++ (show i) ++ "] " ++ (showContext c) ++ "|-" ++ (show e) ++ " " ++ (show t) ++ "\n"
toString (TripleP t e c g1 g2 g3) i = (toString g1 (i + 1)) ++ (toString g2 (i + 1)) ++ (toString g3 (i + 1)) ++ "[" ++ (show i) ++ "] " ++ (showContext c) ++ "|-" ++ (show e) ++ " " ++ (show t) ++ "\n"
 
instance Show ProofTree where
    show p = toString p 0
 
addContext :: Expr -> ProofTree -> ProofTree
addContext e (AxP f c) = (AxP f (e:c))
addContext e (SingleP t f c g) = (SingleP t f (e:c) (addContext e g))
addContext e (BinaryP t f c g1 g2) = (BinaryP t f (e:c) (addContext e g1) (addContext e g2))
addContext e (TripleP t f c g1 g2 g3) = (TripleP t f (e:c) (addContext e g1) (addContext e g2) (addContext e g3))
 
evaluator :: Binop -> Bool -> Bool -> Bool
evaluator And a b = (&&) a b
evaluator Or a b = (||) a b
evaluator Impl a b = (not a) || b
 
getAllVars :: Expr -> (S.Set Expr)
getAllVars (Var s) = S.singleton (Var s)
getAllVars (Binary _ a b) = S.union (getAllVars a) (getAllVars b)
getAllVars FalseE = S.empty
 
buildBinopProof :: Expr -> [Expr] -> (ProofTree, Bool) -> (ProofTree, Bool) -> (ProofTree, Bool)
buildBinopProof (Binary And a b) c (pa, True) (pb, True) = (
        (BinaryP IAndP (a |-& b) c pa pb),
        True
    )
 
buildBinopProof (Binary And a b) c _ (pb, False) = (
        (SingleP IIMplP ((a |-& b) |--> _l_) c 
            (BinaryP EImplP _l_ ((a |-& b):c) 
                (addContext (a |-& b) pb)
                (SingleP ErAndP b ((a |-& b):c) 
                    (AxP (a |-& b) ((a |-& b):c))
                )
            )
        ),
        False
    )

buildBinopProof (Binary And a b) c (pa, False) _ = (
        (SingleP IIMplP ((a |-& b) |--> _l_) c 
            (BinaryP EImplP _l_ ((a |-& b):c) 
                (addContext (a |-& b) pa)
                (SingleP ElAndP a ((a |-& b):c) 
                    (AxP (a |-& b) ((a |-& b):c))
                )
            )
        ),
        False
    )
 
 
buildBinopProof (Binary Or a b) c (pa, True) _ = (
        (SingleP IlOrP (a |-| b) c pa),
        True
    )
 
buildBinopProof (Binary Or a b) c _ (pb, True) = (
        (SingleP IrOrP (a |-| b) c pb),
        True
    )
 
buildBinopProof (Binary Or a b) c (pa, False) (pb, False) = (
        (SingleP IIMplP ((a |-| b) |--> _l_) c 
            (TripleP EOrP _l_ ((a |-| b):c)
                (BinaryP EImplP _l_ (a:(a |-| b):c)
                    (addContext a . addContext (a |-| b) $ pa)
                    (AxP a (a:(a |-| b):c))
                )
                (BinaryP EImplP _l_ (b:(a |-| b):c)
                    (addContext b . addContext (a |-| b) $ pb)
                    (AxP b (b:(a |-| b):c))
                )
                (AxP (a |-| b) ((a |-| b):c))
            )
        ),
        False
    )
 
buildBinopProof (Binary Impl a b) c _ (pb, True) = (
        (SingleP IIMplP (a |--> b) c (addContext a pb)),
        True
    )
 
buildBinopProof (Binary Impl a b) c (pa, False) _ = (
        (SingleP IIMplP (a |--> b) c
            (BinaryP EImplP b (a:c)
                (SingleP IIMplP (_l_ |--> b) (a:c)
                    (SingleP ENotP b (_l_:a:c)
                        (AxP _l_ ((b |--> _l_):_l_:a:c))
                    )
                )
                (BinaryP EImplP _l_ (a:c)
                    (addContext a pa)
                    (AxP a (a:c))
                )
            )
        ),
        True
    )
 
buildBinopProof (Binary Impl a b) c (pa, True) (pb, False) = (
        (SingleP IIMplP ((a |--> b) |--> _l_) c
            (BinaryP EImplP _l_ ((a |--> b):c)
                (addContext (a |--> b) pb)
                (BinaryP EImplP b ((a |--> b):c)
                    (AxP (a |--> b) ((a |--> b):c))
                    (addContext (a |--> b) pa)
                )
            )
        ),
        False
    )
 
contextedProof :: [Expr] -> Expr -> (ProofTree, Bool)
contextedProof c (Var s) = if (elem (Var s) c) then ((AxP (Var s) c), True) else ((AxP ((Var s) |--> _l_) c), False)
contextedProof c FalseE = ((SingleP IIMplP (_l_ |--> _l_) c (AxP _l_ (_l_:c))), False)
contextedProof c (Binary op a b) = buildBinopProof (Binary op a b) c (contextedProof c a) (contextedProof c b)
 
proof :: [Expr] -> Expr -> [Expr] -> (ProofTree, [Expr], Bool)
proof [] e c = let (rt, rv) = contextedProof c e in (rt, c, rv)
proof (x:xs) e c = let (rtt, rtc, rtv) = proof xs e (x:c) in let (rft, rfc, rfv) = proof xs e ((x |--> _l_):c) in
    if (not rtv) then (rtt, rtc, rtv)
    else if (not rfv) then (rft, rfc, rfv)
    else 
    (
        (TripleP EOrP e c
            rtt
            rft
            (SingleP ENotP (x |-| (x |--> _l_)) c (addContext (x |-| (x |--> _l_) |--> _l_)
                (BinaryP EImplP _l_ c
                    (BinaryP EImplP ((x |--> _l_) |--> _l_) c
                        (BinaryP EImplP ((x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_) |--> _l_) c
                            (SingleP IIMplP (((x |--> _l_) |--> x |-| (x |--> _l_)) |--> (x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_) |--> _l_) c (addContext ((x |--> _l_) |--> x |-| (x |--> _l_))
                                (SingleP IIMplP ((x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_) |--> _l_) c (addContext (x |-| (x |--> _l_) |--> _l_)
                                    (SingleP IIMplP ((x |--> _l_) |--> _l_) c (addContext (x |--> _l_)
                                        (BinaryP EImplP _l_ c
                                            (AxP (x |-| (x |--> _l_) |--> _l_) c)
                                            (SingleP IrOrP (x |-| (x |--> _l_)) c
                                                (AxP (x |--> _l_) c)
                                            )
                                        )
                                    ))
                                ))
                            ))
                            (SingleP IIMplP ((x |--> _l_) |--> x |-| (x |--> _l_)) c (addContext (x |--> _l_)
                                (SingleP IrOrP (x |-| (x |--> _l_)) c
                                    (AxP (x |--> _l_) c)
                                )
                            ))
                        )
                        (AxP (x |-| (x |--> _l_) |--> _l_) c)
                    )
                    (BinaryP EImplP (x |--> _l_) c
                        (BinaryP EImplP ((x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_)) c
                            (SingleP IIMplP ((x |--> x |-| (x |--> _l_)) |--> (x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_)) c (addContext (x |--> x |-| (x |--> _l_)) 
                                (SingleP IIMplP ((x |-| (x |--> _l_) |--> _l_) |--> (x |--> _l_)) c (addContext (x |-| (x |--> _l_) |--> _l_) 
                                    (SingleP IIMplP (x |--> _l_) c (addContext x
                                        (BinaryP EImplP _l_ c
                                            (AxP (x |-| (x |--> _l_) |--> _l_) c)
                                            (SingleP IlOrP (x |-| (x |--> _l_)) c
                                                (AxP x c)
                                            )
                                        )
                                    ))
                                ))
                            ))
                            (SingleP IIMplP (x |--> x |-| (x |--> _l_)) c (addContext x
                                (SingleP IlOrP (x |-| (x |--> _l_)) c
                                    (AxP x c)
                                )
                            ))
                        )
                        (AxP (x |-| (x |--> _l_) |--> _l_) c)
                    )
                ))
            )
        ),
        c,
        True
    )
 
-- [END] ProofTree
 
printEval :: Expr -> String
printEval (Var s) = s ++ ":=T"
printEval (Binary _ (Var s) FalseE) = s ++ ":=F"
 
printErr :: [Expr] -> String
printErr e = "Formula is refutable [" ++ (intercalate "," . map printEval $ e) ++ "]\n"
 
main = do
    input <- getLine
    let expr = parseS input
    let (res, hyp, val) = proof (S.toList . getAllVars $ expr) expr []
    putStr (if (val) then (show res) else (printErr hyp))
