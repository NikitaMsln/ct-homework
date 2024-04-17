module Main where
  
{-# LANGUAGE Strict #-}
  
import Data.List (sort)
import System.IO (isEOF)
import Data.Set (empty, member, union, singleton, insert, Set)
import Data.Text (Text, pack, append)
import Data.Monoid (mconcat)
import Text.Printf
     
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
  show (Binary op a b) = "(" ++ (show a) ++ (show op) ++ (show b) ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name
 
toText :: Expr -> Text
toText (Not e) = mconcat [(pack "(!"), toText e, (pack ")")]
toText (Var n) = (pack n)
toText (Binary op a b) = mconcat [(pack "("), toText a, (pack . show $ op), toText b, (pack ")")]
   
infixl 6 |-&
infixl 5 |-|
infixr 4 |-->
        
(|-&) :: Expr -> Expr -> Expr
a |-& b = (Binary And a b)
(|-|) :: Expr -> Expr -> Expr
a |-| b = (Binary Or a b)
(|-->) :: Expr -> Expr -> Expr
a |--> b = (Binary Impl a b)
      
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
     
data ProofWay = Axiom Int | Hypothes Int | MP Int Int | Deduction Int | Incorrect deriving (Eq)
     
instance Show ProofWay where
    show (Axiom i) = "[Ax. sch. " ++ (show i) ++ "]"
    show (Hypothes i) = "[Hyp. " ++ (show i) ++ "]"
    show (MP i j) = "[M.P. "++ (show i) ++ ", " ++ (show j) ++ "]"
    show (Deduction i) = "[Ded. " ++ (show i) ++ "]"
    show (Incorrect) = "[Incorrect]"
   
data ProofTree = AxiomProof Expr | HypothesProof Expr | MPProof Expr ProofTree ProofTree
   
getExpr :: ProofTree -> Expr
getExpr (AxiomProof e) = e
getExpr (HypothesProof e) = e
getExpr (MPProof e _ _) = e
   
usedHypothes :: Expr -> ProofTree -> Bool
usedHypothes e (AxiomProof f) = False
usedHypothes e (HypothesProof h) = (e == h)
usedHypothes e (MPProof _ g h) = (usedHypothes e g) || (usedHypothes e h)
   
instance Show ProofTree where
    show (AxiomProof e) = (show e) ++ " [Axiom]\n"
    show (HypothesProof h) = (show h) ++ " [Hypothes]\n"
    show (MPProof m l r) = (show l) ++ (show r) ++ (show m) ++ " [MP]\n"
   
printProofImpl :: ProofTree -> Set Expr -> ([Expr], Set Expr)
printProofImpl (AxiomProof e) s = if (member e s) then ([], s) else ([e], insert e s)
printProofImpl (HypothesProof e) s = if (member e s) then ([], s) else ([e], insert e s)
printProofImpl (MPProof e f g) s = if (member e s) then ([], s) else let (r1, s1) = printProofImpl f s in let (r2, s2) = printProofImpl g s1 in ((e:r2) ++ r1, insert e s2)
   
printProof :: ProofTree -> [Expr]
printProof p = reverse . fst . printProofImpl p $ empty
     
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
   
makeMPProof :: Expr -> ProofTree -> ProofTree -> ProofTree
makeMPProof e l r =
    if ((checkAxiom e axioms 1) == Incorrect) then
        (MPProof e l r)
    else (AxiomProof e)
     
isSamePermutation :: [Expr] -> [Expr] -> Bool
isSamePermutation x y = (sort x) == (sort y)
     
findAxiom :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findAxiom e _ _ = checkAxiom e axioms 1
     
findHypothes :: Expr -> [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr)] -> ProofWay
findHypothes e _ [] = Incorrect
findHypothes e _ ((i, h):hs) = if (e == h) then (Hypothes i) else (findHypothes e [] hs)
   
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
     
findProofs :: [(Int, Expr, [(Int, Expr)])] -> [(Int, Expr, [(Int, Expr)])] -> [ProofWay]
findProofs [] _ = []
findProofs ((i, e, h):es) l = (findProof [findAxiom, findHypothes, findMP, findDeduct] e l h):(findProofs es (l ++ [(i, e, h)]))
   
buildImplExpr :: Expr -> [Expr] -> [Expr] -> (Expr, [Expr], [Expr])
buildImplExpr e [] [] = (e, [], [])
buildImplExpr e [] y = (e, [], y)
buildImplExpr e x [] = (e, x, [])
buildImplExpr e (x:xs) (y:ys) = if (x == y) then buildImplExpr (x |--> e) xs ys else (e, (x:xs), (y:ys))
   
getCommonExprOFDeduction :: Expr -> Expr -> (Expr, [Expr], [Expr])
getCommonExprOFDeduction x y = let (xw, xb) = getImplWay x in let (yw, yb) = getImplWay y in let (re, rf, rt) = buildImplExpr yb (reverse xw) (reverse yw) in (re, reverse rf, rt)
   
expandDeductionsFrom :: [Expr] -> ProofTree -> ProofTree
expandDeductionsFrom [] p = p
expandDeductionsFrom (h:hs) p = let (Binary Impl a b) = getExpr p in
    (expandDeductionsFrom
        hs
        (makeMPProof b
            (HypothesProof h)
            p
        )
    )
   
expandDeductionsToHard :: Expr -> ProofTree -> ProofTree
expandDeductionsToHard e (AxiomProof f) =
    (makeMPProof (e |--> f)
        (AxiomProof f)
        (AxiomProof (f |--> e |--> f))
    )
   
expandDeductionsToHard e (HypothesProof f) =
    if (f == e) then
        (makeMPProof (e |--> e)
            (AxiomProof (e |--> (e |--> e) |--> e))
            (makeMPProof ((e |--> (e |--> e) |--> e) |--> (e |--> e))
                (AxiomProof (e |--> e |--> e))
                (AxiomProof ((e |--> e |--> e) |--> (e |--> (e |--> e) |--> e) |--> (e |--> e)))
            )
        )
    else
        (makeMPProof (e |--> f)
            (HypothesProof f)
            (AxiomProof (f |--> e |--> f))
        )
   
expandDeductionsToHard e (MPProof f g h) =
    if (usedHypothes e (MPProof f g h)) then
        (makeMPProof (e |--> f)
            (expandDeductionsToHard e h)
            (makeMPProof ((e |--> (getExpr h)) |--> (e |--> f))
                (expandDeductionsToHard e g)
                (AxiomProof ((e |--> (getExpr g)) |--> (e |--> (getExpr h)) |--> (e |--> f)))
            )
        )
    else
        (makeMPProof (e |--> f)
            (MPProof f g h)
            (AxiomProof (f |--> e |--> f))
        )
   
   
expandDeductionsTo :: [Expr] -> ProofTree -> ProofTree
expandDeductionsTo [] p = p
expandDeductionsTo (h:hs) p =
    if (usedHypothes h p) then
        (expandDeductionsTo hs $! (expandDeductionsToHard h p))
    else
        (expandDeductionsTo
            hs $!
            (makeMPProof (h |--> (getExpr p))
                p
                (AxiomProof ((getExpr p) |--> h |--> (getExpr p)))
            )
        )
   
expandDeductions :: Expr -> [Expr] -> [Expr] -> ProofTree -> ProofTree
expandDeductions be f t p = expandDeductionsTo t (expandDeductionsFrom f p)
   
buildProofTree :: Int -> [(Int, Expr, [(Int, Expr)])] -> [ProofWay] -> ProofTree
buildProofTree i ((j, e, h):es) ((MP l k):ps) =
    if (i == j) then makeMPProof e (buildProofTree l es ps) (buildProofTree k es ps)
    else buildProofTree i es ps
   
buildProofTree i ((j, e, h):es) ((Axiom _):ps) = if (i == j) then (AxiomProof e) else buildProofTree i es ps
   
buildProofTree i ((j, e, h):es) ((Hypothes _):ps) = if (i == j) then (HypothesProof e) else buildProofTree i es ps
   
buildProofTree i ((j, e, h):es) ((Deduction k):ps) =
    if (i == j) then
        let r = buildProofTree k es ps in let (be, fe, te) = getCommonExprOFDeduction (getExpr r) e in
            expandDeductions be fe te r
    else buildProofTree i es ps
   
parseLines :: [String] -> ProofTree
parseLines s = let r = (map parseLineStr (zip [1..(length s)] $! s)) in buildProofTree (length s) (reverse r) $! (reverse (findProofs r $! []))
     
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
    putStrLn (last input)
    mapM (printf "%s\n" . toText) . printProof . parseLines $ input