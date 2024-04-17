module Main where
    
import Data.List (intercalate)
import Data.Map (Map, empty, singleton, union, insert, member, (!?), fromList, (!))
import System.IO (isEOF)
    
class TokenParse p where
    parse :: String -> [(p, String)]
    
data Token = LeftP | RightP | OrT | AndT | ImplT | NotT | ForallT | ExistT | CommaT | EqT | IncT | PlusT | MultT | DotT | VarT Char | HypT | EmptyT deriving (Show, Eq)
   
isPredChar :: Char -> Bool
isPredChar x = ((fromEnum x) >= (fromEnum 'A') && (fromEnum x) <= (fromEnum 'Z')) || ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9'))
 
isVarChar :: Char -> Bool
isVarChar x = ((fromEnum x) >= (fromEnum 'a') && (fromEnum x) <= (fromEnum 'z')) || ((fromEnum x) >= (fromEnum '0') && (fromEnum x) <= (fromEnum '9'))
   
instance TokenParse Token where
    parse [] = []
    parse ('(':xs) = [(LeftP, xs)]
    parse (')':xs) = [(RightP, xs)]
    parse ('|':'-':xs) = [(HypT, xs)]
    parse ('|':xs) = [(OrT, xs)]
    parse ('&':xs) = [(AndT, xs)]
    parse ('-':'>':xs) = [(ImplT, xs)]
    parse ('!':xs) = [(NotT, xs)]
    parse ('.':xs) = [(DotT, xs)]
    parse ('=':xs) = [(EqT, xs)]
    parse ('\'':xs) = [(IncT, xs)]
    parse ('+':xs) = [(PlusT, xs)]
    parse ('*':xs) = [(MultT, xs)]
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
    
data BinopP = Impl | Or | And deriving (Eq, Ord)
   
instance Show BinopP where
    show Impl = "->"
    show Or = "|"
    show And = "&"
   
data Quantop = Forall | Exist deriving (Eq, Ord)
   
instance Show Quantop where
    show Forall = "@"
    show Exist = "?"
 
data BinopE = Add | Multy deriving (Eq, Ord)
 
instance Show BinopE where
    show Add = "+"
    show Multy = "*"
   
data Expr = BinaryE BinopE Expr Expr |
            Inc Expr |
            Zero |
            Var String deriving (Eq, Ord)
 
data Prop = Quant Quantop String Prop |
            BinaryP BinopP Prop Prop |
            Not Prop |
            Propos String |
            Equ Expr Expr deriving (Eq, Ord)
   
instance Show Expr where
    show (BinaryE op x y) = "(" ++ (show x) ++ (show op) ++ (show y) ++ ")"
    show (Inc x) = (show x) ++ "\'"
    show (Zero) = "0"
    show (Var x) = x
   
instance Show Prop where
    show (Quant q x y) = "(" ++ (show q) ++ x ++ "." ++ (show y) ++ ")"
    show (BinaryP op x y) = "(" ++ (show x) ++ (show op) ++ (show y) ++ ")"
    show (Not x) = "(!" ++ (show x) ++ ")"
    show (Equ x y) = (show x) ++ "=" ++ (show y)
    
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
   
makeBinopP :: BinopP -> [Prop] -> Prop
makeBinopP Impl (e:es) = if (emptyT es) then e else (BinaryP Impl e (makeBinopP Impl es))
makeBinopP b (e:es) = if (emptyT es) then e else (BinaryP b (makeBinopP b es) e)
   
makeBinopE :: BinopE -> [Expr] -> Expr
makeBinopE b (e:es) = if (emptyT es) then e else (BinaryE b (makeBinopE b es) e)
    
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
   
parseExpr :: [Token] -> [(Prop, [Token])]
parseOr :: [Token] -> [(Prop, [Token])]
parseAnd :: [Token] -> [(Prop, [Token])]
parseUnar :: [Token] -> [(Prop, [Token])]
parsePred :: [Token] -> [(Prop, [Token])]
parseTerm :: [Token] -> [(Expr, [Token])]
parsePlus :: [Token] -> [(Expr, [Token])]
parseMult :: [Token] -> [(Expr, [Token])]
parseVar :: [Token] -> [(Expr, [Token])]
   
parseVar = convert (\x -> if (x == "0") then Zero else (Var x)) . clear (\x -> x /= "") . parseSeq (filterVar isVarChar) (\(VarT c) -> c)
 
makeInc :: Expr -> Int -> Expr
makeInc e 0 = e
makeInc e n = makeInc (Inc e) (n - 1)
 
parseMult = parseCombine
    (parseTry [
        parseVar,
        parseCombine (parseToken (isToken LeftP) (\x -> ())) (parseCombine parseTerm (parseToken (isToken RightP) (\x -> ())) fstArg) sndArg 
    ])
    (parseOptional 0 (parseWhileCan (parseToken (isToken IncT) (\x -> 1)) (\x -> [((), x)]) sum))
    makeInc
 
parsePlus = (parseCombine parseMult (parseOptional [] (parseCombine (parseToken (isToken MultT) (\x -> ())) (parseWhileCan parseMult (parseToken (isToken MultT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinopE Multy . reverse $ (x:y)))
 
parseTerm = (parseCombine parsePlus (parseOptional [] (parseCombine (parseToken (isToken PlusT) (\x -> ())) (parseWhileCan parsePlus (parseToken (isToken PlusT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinopE Add . reverse $ (x:y)))
   
parsePred = parseCombine (parseCombine parseTerm (parseToken (isToken EqT) (\x -> ())) fstArg) parseTerm (\x y -> Equ x y)
 
parseUnar (ForallT:ts) = parseCombine parseVar (parseCombine (parseToken (isToken DotT) (\x -> ())) parseExpr sndArg) (\(Var x) y -> (Quant Forall x y)) ts
parseUnar (ExistT:ts) = parseCombine parseVar (parseCombine (parseToken (isToken DotT) (\x -> ())) parseExpr sndArg) (\(Var x) y -> (Quant Exist x y)) ts
parseUnar (NotT:ts) = convert (\x -> Not x) . parseUnar $ ts
parseUnar (LeftP:ts) = parseCombine parseExpr (parseToken (isToken RightP) (\x -> ())) fstArg ts
parseUnar t = parsePred t
    
parseAnd = (parseCombine parseUnar (parseOptional [] (parseCombine (parseToken (isToken AndT) (\x -> ())) (parseWhileCan parseUnar (parseToken (isToken AndT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinopP And . reverse $ (x:y)))
    
parseOr = (parseCombine parseAnd (parseOptional [] (parseCombine (parseToken (isToken OrT) (\x -> ())) (parseWhileCan parseAnd (parseToken (isToken OrT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinopP Or . reverse $ (x:y)))
    
parseExpr = (parseCombine parseOr (parseOptional [] (parseCombine (parseToken (isToken ImplT) (\x -> ())) (parseWhileCan parseOr (parseToken (isToken ImplT) (\x -> ())) (\x -> x)) sndArg)) (\x y -> makeBinopP Impl (x:y)))
   
parseS :: String -> Prop
parseS x = valueT (parseExpr (filter (\x -> (x /= EmptyT)) (parseTokens x)))
 
parseHyp :: String -> ([Prop], Prop)
parseHyp = valueT . parseCombine (parseOptional [] (parseWhileCan parseExpr (parseToken (isToken CommaT) (\x->())) (\x -> x))) (parseCombine (parseToken (isToken HypT) (\x -> ())) parseExpr sndArg) (\x y -> (x, y)) . filter (\x -> x /= EmptyT) . parseTokens
 
-- [BEING] Proof checker
 
infixl 6 |-&
infixl 5 |-|
infixr 4 |-->
 
infixl 7 |-=
infixl 8 |-+
infixl 9 |-*
         
(|-&) :: Prop -> Prop -> Prop
a |-& b = (BinaryP And a b)
(|-|) :: Prop -> Prop -> Prop
a |-| b = (BinaryP Or a b)
(|-->) :: Prop -> Prop -> Prop
a |--> b = (BinaryP Impl a b)
         
(|-=) :: Expr -> Expr -> Prop
a |-= b = (Equ a b)
(|-+) :: Expr -> Expr -> Expr
a |-+ b = (BinaryE Add a b)
(|-*) :: Expr -> Expr -> Expr
a |-* b = (BinaryE Multy a b)
 
-- [BEING] Axiom checker

ap1 = Propos "1" :: Prop
ap2 = Propos "2" :: Prop
ap3 = Propos "3" :: Prop
 
logicAx = [
        (ap1 |--> ap2 |--> ap1),
        ((ap1 |--> ap2) |--> (ap1 |--> ap2 |--> ap3) |--> ap1 |--> ap3),
        (ap1 |-& ap2 |--> ap1),
        (ap1 |-& ap2 |--> ap2),
        (ap1 |--> ap2 |--> ap1 |-& ap2),
        (ap1 |--> ap1 |-| ap2),
        (ap2 |--> ap1 |-| ap2),
        ((ap1 |--> ap3) |--> (ap2 |--> ap3) |--> (ap1 |-| ap2 |--> ap3)),
        ((ap1 |--> ap2) |--> (ap1 |--> (Not ap2)) |--> (Not ap1)),
        ((Not (Not ap1)) |--> ap1)
    ] :: [Prop]
 
av1 = Var "1"
av2 = Var "2"
av3 = Var "3"
 
unlogicAx = [
        av1 |-= av2 |--> av1 |-= av3 |--> av2 |-= av3,
        av1 |-= av2 |--> (Inc av1) |-= (Inc av2),
        (Inc av1) |-= (Inc av2) |--> av1 |-= av2,
        (Not ((Inc av1) |-= Zero)),
        av1 |-+ Zero |-= av1,
        av1 |-+ (Inc av2) |-= (Inc (av1 |-+ av2)),
        av1 |-* Zero |-= Zero,
        av1 |-* (Inc av2) |-= av1 |-* av2 |-+ av1
    ]
 
checkSingleton :: Eq a => Maybe a -> a -> Bool
checkSingleton Nothing e = True
checkSingleton (Just a) b = a == b
 
checkPropSubst :: Prop -> Prop -> (Map String Prop) -> (Bool, Map String Prop)
checkPropSubst (Propos s) e m = if ((Propos s) == e) then (True, empty) else (checkSingleton (m !? s) e, insert s e m)
checkPropSubst (Quant op n e) (Quant ops ns es) m = let (v, r) = checkPropSubst e es m in (op == ops && n == ns && v, r)
checkPropSubst (BinaryP op a b) (BinaryP ops as bs) m = (va && vb && (op == ops), rb) where
    (va, ra) = checkPropSubst a as m
    (vb, rb) = checkPropSubst b bs ra
checkPropSubst (Not e) (Not es) m = checkPropSubst e es m
checkPropSubst a b m = (a == b, m)
 
checkExprSubstIn :: Expr -> Expr -> (Map String Expr) -> (Bool, Map String Expr)
checkExprSubstIn (Var s) e m = if ((Var s) == e) then (True, empty) else (checkSingleton (m !? s) e, insert s e m)
checkExprSubstIn (BinaryE op a b) (BinaryE ops as bs) m = (va && vb && (op == ops), rb) where
    (va, ra) = checkExprSubstIn a as m
    (vb, rb) = checkExprSubstIn b bs ra
checkExprSubstIn (Inc e) (Inc es) m = checkExprSubstIn e es m
checkExprSubstIn a b m = (a == b, m)
 
checkExprSubstOut :: Prop -> Prop -> (Map String Expr) -> (Bool, Map String Expr)
checkExprSubstOut (Quant op n e) (Quant ops ns es) m = let (v, r) = checkExprSubstOut e es m in (op == ops && n == ns && v, r)
checkExprSubstOut (BinaryP op a b) (BinaryP ops as bs) m = (va && vb && (op == ops), rb) where
    (va, ra) = checkExprSubstOut a as m
    (vb, rb) = checkExprSubstOut b bs ra
checkExprSubstOut (Not e) (Not es) m = checkExprSubstOut e es m
checkExprSubstOut (Equ a b) (Equ as bs) m = (va && vb, rb) where
    (va, ra) = checkExprSubstIn a as m
    (vb, rb) = checkExprSubstIn b bs ra
checkExprSubstOut a b m = (a == b, m)
    
checkSubstEquality :: Eq a => Maybe a -> Maybe a -> (Bool, Maybe a)
checkSubstEquality a Nothing = (True, a)
checkSubstEquality Nothing b = (True, b)
checkSubstEquality (Just a) (Just b) = (a == b, Just a)
 
checkSubstitutionIn :: String -> Expr -> Expr -> (Bool, Maybe Expr)
checkSubstitutionIn s (Var ss) es = if (s == ss) then (True, Just es) else ((Var ss) == es, Nothing)
checkSubstitutionIn s (BinaryE op a b) (BinaryE ops as bs) = (va && vb && (op == ops) && v, r) where
    (va, ra) = checkSubstitutionIn s a as
    (vb, rb) = checkSubstitutionIn s b bs
    (v, r) = checkSubstEquality ra rb
checkSubstitutionIn s (Inc es) (Inc xs) = checkSubstitutionIn s es xs
checkSubstitutionIn _ Zero Zero = (True, Nothing)
checkSubstitutionIn _ _ _ = (False, Nothing)
 
checkSubstitutionOut :: String -> Prop -> Prop -> (Bool, Maybe Expr)
checkSubstitutionOut s (Quant t a b) (Quant ts as bs) =
    if (a == s) then
        ((ts == t) && (a == as) && (b == bs), Nothing)
    else
        (v, r) where
            (yv, r) = checkSubstitutionOut s b bs
            v = (t == ts) && (a == as) && yv
checkSubstitutionOut s (BinaryP op a b) (BinaryP ops as bs) =
    (v, r) where
        (xv, xr) = checkSubstitutionOut s a as
        (yv, yr) = checkSubstitutionOut s b bs
        (rv, r) = checkSubstEquality xr yr
        v = (op == ops) && xv && yv && rv
checkSubstitutionOut s (Not a) (Not as) = checkSubstitutionOut s a as
checkSubstitutionOut s (Equ a b) (Equ as bs) =
    (v, r) where
        (xv, xr) = checkSubstitutionIn s a as
        (yv, yr) = checkSubstitutionIn s b bs
        (rv, r) = checkSubstEquality xr yr
        v = xv && yv && rv
checkSubstitutionOut _ _ _ = (False, Nothing)
 
    
data IndependenceTree = Node [IndependenceTree] |
                        VarLeaf Bool deriving (Eq)
    
buildIndependenceTreeImplIn :: (Map String ()) -> Expr -> IndependenceTree
buildIndependenceTreeImplIn s (Var a) = VarLeaf (member a s)
buildIndependenceTreeImplIn s (BinaryE op a b) = Node [(buildIndependenceTreeImplIn s a), (buildIndependenceTreeImplIn s b)]
buildIndependenceTreeImplIn s (Inc a) = Node [(buildIndependenceTreeImplIn s a)]
buildIndependenceTreeImplIn s Zero = VarLeaf True
 
buildIndependenceTreeImplOut :: (Map String ()) -> Prop -> IndependenceTree
buildIndependenceTreeImplOut s (BinaryP op a b) = Node [(buildIndependenceTreeImplOut s a), (buildIndependenceTreeImplOut s b)]
buildIndependenceTreeImplOut s (Quant t a b) = Node [(buildIndependenceTreeImplOut (insert a () s) b)]
buildIndependenceTreeImplOut s (Not a) = Node [(buildIndependenceTreeImplOut s a)]
buildIndependenceTreeImplOut s (Equ a b) = Node [(buildIndependenceTreeImplIn s a), (buildIndependenceTreeImplIn s b)]
 
buildIndependenceTreeIn :: Expr -> IndependenceTree
buildIndependenceTreeIn = buildIndependenceTreeImplIn empty
 
buildIndependenceTreeOut :: Prop -> IndependenceTree
buildIndependenceTreeOut = buildIndependenceTreeImplOut empty
 
checkSubstIndependenceIn :: String -> Expr -> IndependenceTree -> Expr -> IndependenceTree -> Bool
checkSubstIndependenceIn s (Var a) t as ts = (s /= a) || (t == ts)
checkSubstIndependenceIn s (BinaryE op a b) t (BinaryE ops as bs) (Node (axs:bxs:xss)) = (checkSubstIndependenceIn s a t as axs) && (checkSubstIndependenceIn s b t bs bxs)
checkSubstIndependenceIn s (Inc a) t (Inc as) (Node (axs:xss)) = checkSubstIndependenceIn s a t as axs
checkSubstIndependenceIn s Zero _ Zero _ = True
 
checkSubstIndependenceOut :: String -> Prop -> IndependenceTree -> Prop -> IndependenceTree -> Bool
checkSubstIndependenceOut s (BinaryP op a b) t (BinaryP ops as bs) (Node (axs:bxs:xss)) = (checkSubstIndependenceOut s a t as axs) && (checkSubstIndependenceOut s b t bs bxs)
checkSubstIndependenceOut s (Quant _ a b) t (Quant _ as bs) (Node (bxs:xss)) = (s == a && b == bs) || (checkSubstIndependenceOut s b t bs bxs)
checkSubstIndependenceOut s (Not a) t (Not as) (Node (axs:xss)) = checkSubstIndependenceOut s a t as axs
checkSubstIndependenceOut s (Equ a b) t (Equ as bs) (Node (axs:bxs:xss)) = (checkSubstIndependenceIn s a t as axs) && (checkSubstIndependenceIn s b t bs bxs)
 
choseJust :: Maybe a -> a -> a
choseJust Nothing x = x
choseJust (Just x) _ = x
 
isAxiom11 :: Prop -> Bool
isAxiom11 (BinaryP Impl (Quant Forall x phi) oth) =
    let (rv, re) = checkSubstitutionOut x phi oth in
        if (rv) then
            checkSubstIndependenceOut x phi (buildIndependenceTreeIn (choseJust re (Var x))) oth (buildIndependenceTreeOut oth)
        else False
isAxiom11 _ = False
 
isAxiom12 :: Prop -> Bool
isAxiom12 (BinaryP Impl oth (Quant Exist x phi)) =
    let (rv, re) = checkSubstitutionOut x phi oth in
        if (rv) then
            checkSubstIndependenceOut x phi (buildIndependenceTreeIn (choseJust re (Var x))) oth (buildIndependenceTreeOut oth)
        else False
isAxiom12 _ = False
 
isAxiomA9 :: Prop -> Bool
isAxiomA9 (BinaryP Impl (BinaryP And phi0 (Quant Forall x (BinaryP Impl phix phinx))) phiy) = 
    (v1 && v2 && (phix == phiy) && (r1 == Just Zero) && (r2 == Just (Inc (Var x)))) where
        (v1, r1) = checkSubstitutionOut x phix phi0
        (v2, r2) = checkSubstitutionOut x phix phinx
isAxiomA9 _ = False
 
isLogicalAxImpl :: Int -> [Prop] -> Prop -> Maybe ([Int], String)
isLogicalAxImpl i (x:xs) e = if (fst . checkPropSubst x e $ empty) then Just ([], "Ax. sch. " ++ (show i)) else isLogicalAxImpl (i + 1) xs e
isLogicalAxImpl _ [] _ = Nothing
 
isLogicalAx :: Prop -> Maybe ([Int], String)
isLogicalAx = isLogicalAxImpl 1 logicAx
 
isUnlogicalAxImpl :: Int -> [Prop] -> Prop -> Maybe ([Int], String)
isUnlogicalAxImpl i (x:xs) e = if (fst . checkExprSubstOut x e $ empty) then Just ([], "Ax. A" ++ (show i)) else isUnlogicalAxImpl (i + 1) xs e
isUnlogicalAxImpl _ [] _ = Nothing
 
isUnlogicalAx :: Prop -> Maybe ([Int], String)
isUnlogicalAx = isUnlogicalAxImpl 1 unlogicAx
 
isAxiom11V :: Prop -> Maybe ([Int], String)
isAxiom11V p = if (isAxiom11 p) then Just ([], "Ax. sch. 11") else Nothing
 
isAxiom12V :: Prop -> Maybe ([Int], String)
isAxiom12V p = if (isAxiom12 p) then Just ([], "Ax. sch. 12") else Nothing
 
isAxiomA9V :: Prop -> Maybe ([Int], String)
isAxiomA9V p = if (isAxiomA9 p) then Just ([], "Ax. sch. A9") else Nothing
 
-- [BEING] Hyp, MP, ?/@ checkers
 
isCorr :: Maybe a -> Bool
isCorr Nothing = False
isCorr _ = True
 
chooseCorrect :: [(a -> Maybe b)] -> a -> Maybe b
chooseCorrect (x:xs) y = let r = x y in if (isCorr r) then r else chooseCorrect xs y
chooseCorrect [] _ = Nothing
 
findHypothes :: Int -> [Prop] -> Prop -> Maybe ([Int], String)
findHypothes i (x:xs) e = if (x == e) then Just ([], "Hyp. " ++ (show i)) else findHypothes (i + 1) xs e
findHypothes i [] e = Nothing
 
findMPPart :: Int -> Prop -> [(Int, Prop)] -> Maybe ([Int], String)
findMPPart l p ((k, e):t) = if (p == e) then Just ([l, k], "M.P. " ++ (show k) ++ ", " ++ (show l)) else findMPPart l p t
findMPPart _ _ [] = Nothing
 
findOutRule :: [(Int, Prop)] -> [(Int, Prop)] -> Prop -> Maybe ([Int], String)
findOutRule ((i, (BinaryP Impl a b)):t) ts (BinaryP Impl as (Quant Forall x bs)) = if (a == as && b == bs) then Just ([i], "@-rule " ++ (show i)) else findOutRule t ts (BinaryP Impl as (Quant Forall x bs))
findOutRule ((i, (BinaryP Impl a b)):t) ts (BinaryP Impl (Quant Exist x as) bs) = if (a == as && b == bs) then Just ([i], "?-rule " ++ (show i)) else findOutRule t ts (BinaryP Impl (Quant Exist x as) bs)
findOutRule ((i, (BinaryP Impl a b)):t) ts e = if (b == e) then (let r = findMPPart i a ts in if (r == Nothing) then findOutRule t ts a else r) else findOutRule t ts e
findOutRule (a:t) ts e = findOutRule t ts e
findOutRule [] _ _ = Nothing
 
checkProof :: [(Int, Prop)] -> [Prop] -> Maybe ([Int], String)
checkProof ((i, x):xs) h = chooseCorrect [findHypothes 1 h, findOutRule xs xs, isLogicalAx, isUnlogicalAx, isAxiom11V, isAxiom12V, isAxiomA9V] x

-- [BEGIN] Proof lines
 
fillMap :: Ord a => [a] -> Map a () -> Map a ()
fillMap (x:xs) m = insert x () (fillMap xs m)
fillMap [] m = m

fillAs :: a -> [b] -> [a] -> [a]
fillAs y (x:xs) ys = fillAs y xs (y:ys)
fillAs y [] ys = ys
 
markProof :: [(Int, Prop)] -> [(Int, Prop)] -> [Prop] -> (Map Prop ()) -> Prop -> ([[Int]], [String]) -> (Int, [[Int]], [String])
markProof ((i, x):xs) ys h pm e (ans1, ans2) =
    if (not . member x $ pm) then
        (let (j, s) = choseJust (checkProof ((i, x):ys) h) ([], "Redundant") in 
            if (x == e && s /= "Redundant") then
                (i, (fillAs [] xs (j:ans1)), (fillAs "Redundant" xs (s:ans2)))
            else
                markProof xs (if (s /= "Redundant") then (i, x):ys else ys) h (if (s /= "Redundant") then (insert x () pm) else pm) e (j:ans1, s:ans2)
                )
    else
        markProof xs ys h pm e ([]:ans1, "Redundant":ans2)
 
showProof :: Int -> [String] -> [String] -> String
showProof i (a:as) (b:bs) = "[" ++ (show i) ++ ". " ++ b ++ "] " ++ a ++ "\n" ++ (showProof (i + 1) as bs)
showProof i [] [] = ""
 
findElemImpl :: Eq a => Int -> a -> [a] -> Int
findElemImpl i x (y:ys) = if (x == y) then i else findElemImpl (i - 1) x ys
findElemImpl _ _ [] = -1

findElem :: Eq a => a -> [a] -> Int
findElem a b = findElemImpl (length b) a b

redundantFilter :: Map Int () -> [(Int, Prop)] -> [String] -> [String]
redundantFilter m ((i, x):xs) (s:ss) = if (member i m) then (s:(redundantFilter m xs ss)) else ("Redundant":(redundantFilter m xs ss))
redundantFilter m [] [] = []
   
fold :: (a -> a -> a) -> a -> [a] -> a
fold f a (x:xs) = f x (fold f a xs)
fold f a _ = a

recursiveFill :: (Map Int [Int]) -> Int -> Map Int ()
recursiveFill st i = fold union (singleton i ()) (map (recursiveFill st) (st ! i))
 
findProof :: Prop -> [String] -> [Prop] -> [String]
findProof e s h = let ps = (zip [1..(length s)] (map parseS s)) in let (i, st, ans) = markProof ps [] h empty e ([], []) in redundantFilter (recursiveFill (fromList (zip [1..(length s)] (reverse st))) i) ps (reverse ans)
   
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
    let (hyp, e) = parseHyp (head input)
    putStrLn (head input)
    let pr = tail $ input
    putStr (showProof 1 pr . findProof e pr $ hyp)
