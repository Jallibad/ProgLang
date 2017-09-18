import PA1Helper
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp

alpha :: Lexp -> Set.Set String -> Lexp
alpha = Set.foldl (\b capture -> replaceCaptures b capture $ until (flip Set.notMember $ variables b) (++"1") capture)

replace :: Lexp -> Lexp -> Lexp -> Lexp
replace orig@(Atom _) old new = if orig == old then new else orig
replace (Lambda s n) old@(Atom s') new@(Atom x) = Lambda (if s == s' then x else s) $ replace n old new
replace (Lambda s n) old new = Lambda s $ replace n old new
replace (Apply l1 l2) old new = Apply (replace l1 old new) (replace l2 old new)

replaceFree :: Lexp -> Lexp -> Lexp -> Lexp
replaceFree orig@(Atom _) old new = if orig == old then new else orig
replaceFree (Lambda s n) old@(Atom s') new
	| s == s' = Lambda s n
	| otherwise = Lambda s $ replaceFree n old new
replaceFree (Apply l1 l2) old new = Apply (replaceFree l1 old new) (replaceFree l2 old new)

replaceCaptures :: Lexp -> String -> String -> Lexp
replaceCaptures orig@(Atom _) _ _ = orig
replaceCaptures orig@(Lambda s n) old new
	| s == old = replace orig (Atom old) (Atom new)
	| otherwise = Lambda s $ replaceCaptures n old new
replaceCaptures (Apply l1 l2) old new = Apply (replaceCaptures l1 old new) (replaceCaptures l2 old new)

variables :: Lexp -> Set.Set String
variables (Atom s) = Set.singleton s
variables (Lambda s lexp) = Set.insert s $ variables lexp
variables (Apply l1 l2) = Set.union (variables l1) (variables l2)

freeVariables :: Lexp -> Set.Set String
freeVariables (Atom s) = Set.singleton s
freeVariables (Lambda s lexp) = Set.delete s $ freeVariables lexp
freeVariables (Apply l1 l2) = Set.union (freeVariables l1) (freeVariables l2)

freeVariable :: String -> Lexp -> Bool
freeVariable s (Atom s') = s == s'
freeVariable s (Lambda s' lexp) = (s /= s') && freeVariable s lexp
freeVariable s (Apply l1 l2) = (freeVariable s l1) || (freeVariable s l2)

beta :: Lexp -> (Lexp, Bool)
beta (Apply l@(Lambda var body) val) = (replaceFree (alpha body captures) (Atom var) val, True)
	where captures = Set.intersection (variables l) (freeVariables val)
beta (Apply m e) = (Apply m' e', t1 || t2)
	where	(m', t1) = beta m
		(e', t2) = beta e
beta orig = (orig, False)

etaReduce :: Lexp -> (Lexp, Bool)
etaReduce orig@(Lambda s (Apply m (Atom s')))
	| (s == s') && (not $ freeVariable s m) = (m, True)
	| otherwise = etaReduce m
etaReduce (Apply m e) = (Apply m' e', t1 || t2)
	where	(m', t1) = etaReduce m
		(e', t2) = etaReduce e
etaReduce orig = (orig, False)

input1 = Apply (Lambda "x" $ Lambda "y" (Apply (Atom "y") (Atom "x"))) (Apply (Atom "y") (Atom "w"))
input8 = Apply (Lambda "y" $ Apply (Lambda "x" $ Lambda "y" (Apply (Atom "x") (Atom "y"))) (Atom "y")) (Apply (Atom "y") (Atom "w"))

evaluater lexp = fst $ last $ takeWhile snd $ iterate (etaReduce . fst) $ last $ takeWhile snd $ iterate (beta . fst) (lexp, True)

-- Entry point of program
main = do
    args <- getArgs
    let filename = fromMaybe "input.lambda" $ listToMaybe args
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram filename evaluater
