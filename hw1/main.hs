import PA1Helper
import System.Environment (getArgs)
import Data.Maybe
import qualified Data.Set as Set

alpha :: Lexp -> Set.Set String -> Lexp
alpha = Set.foldl (\b capture -> replaceCaptures b capture $ until (flip Set.notMember $ variables b) (++"1") capture)

-- Replaces all variables matching the second argument with the third argument
replace :: Lexp -> Lexp -> Lexp -> Lexp
replace orig@(Atom _) old new = if orig == old then new else orig
replace (Lambda s n) old@(Atom s') new@(Atom x) = Lambda (if s == s' then x else s) $ replace n old new
replace (Lambda s n) old new = Lambda s $ replace n old new
replace (Apply l1 l2) old new = Apply (replace l1 old new) (replace l2 old new)

-- Replaces any free variables but skips bound variables
replaceFree :: Lexp -> Lexp -> Lexp -> Lexp
replaceFree orig@(Atom _) old new = if orig == old then new else orig
replaceFree (Lambda s n) old@(Atom s') new
	| s == s' = Lambda s n
	| otherwise = Lambda s $ replaceFree n old new
replaceFree (Apply l1 l2) old new = Apply (replaceFree l1 old new) (replaceFree l2 old new)

-- Replaces any bound variables after skipping free ones
replaceCaptures :: Lexp -> String -> String -> Lexp
replaceCaptures orig@(Atom _) _ _ = orig
replaceCaptures orig@(Lambda s n) old new
	| s == old = replace orig (Atom old) (Atom new)
	| otherwise = Lambda s $ replaceCaptures n old new
replaceCaptures (Apply l1 l2) old new = Apply (replaceCaptures l1 old new) (replaceCaptures l2 old new)

-- Returns the Set of all variables in a given Lexp
variables :: Lexp -> Set.Set String
variables (Atom s) = Set.singleton s
variables (Lambda s lexp) = Set.insert s $ variables lexp
variables (Apply l1 l2) = Set.union (variables l1) (variables l2)

-- Returns the Set of free variables in a given Lexp
freeVariables :: Lexp -> Set.Set String
freeVariables (Atom s) = Set.singleton s
freeVariables (Lambda s lexp) = Set.delete s $ freeVariables lexp
freeVariables (Apply l1 l2) = Set.union (freeVariables l1) (freeVariables l2)

-- Checks if a given variable occurs as a free variable in a Lexp
freeVariable :: String -> Lexp -> Bool
freeVariable s (Atom s') = s == s'
freeVariable s (Lambda s' lexp) = (s /= s') && freeVariable s lexp
freeVariable s (Apply l1 l2) = (freeVariable s l1) || (freeVariable s l2)

-- Performs an beta reduction, returns either Just the simplification,
-- or Nothing if there is no applicable beta reduction
-- Technically this can perform multiple reductions for each Apply statement
beta :: Lexp -> Maybe Lexp
beta (Apply l@(Lambda var body) val) = Just $ replaceFree (alpha body captures) (Atom var) val
	where captures = Set.intersection (variables l) (freeVariables val)
beta (Apply m e) = if isNothing m' && isNothing e' then Nothing else Just (Apply (fromMaybe m m') (fromMaybe e e'))
	where	m' = beta m
		e' = beta e
beta _ = Nothing

-- Performs an eta reduction, returns either Just the simplification,
-- or Nothing if there is no applicable eta reduction
-- Technically this can perform multiple reductions for each Apply statement
etaReduce :: Lexp -> Maybe Lexp
etaReduce orig@(Lambda s (Apply m (Atom s')))
	| (s == s') && (not $ freeVariable s m) = Just m
	| otherwise = etaReduce m
etaReduce (Lambda _ m) = etaReduce m
-- This is ugly and potentially applies two reductions in one
etaReduce (Apply m e) = if isNothing m' && isNothing e' then Nothing else Just (Apply (fromMaybe m m') (fromMaybe e e'))
	where	m' = etaReduce m
		e' = etaReduce e
etaReduce _ = Nothing

input1 = Apply (Lambda "x" $ Lambda "y" (Apply (Atom "y") (Atom "x"))) (Apply (Atom "y") (Atom "w"))
input8 = Apply (Lambda "y" $ Apply (Lambda "x" $ Lambda "y" (Apply (Atom "x") (Atom "y"))) (Atom "y")) (Apply (Atom "y") (Atom "w"))

-- Repeatedly applies beta, then eta reductions to a given expression until it
-- is fully simplified
reducer = fromJust . repeatReduce etaReduce . repeatReduce beta . Just
	where repeatReduce f = last . takeWhile isJust . iterate (>>= f)

-- Entry point of program
main = do
	args <- getArgs
	let filename = fromMaybe "input.lambda" $ listToMaybe args
	runProgram filename reducer
